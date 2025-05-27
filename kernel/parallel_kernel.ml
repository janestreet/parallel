open! Base
open! Import
module Hlist = Hlist
module Panic = Panic
module Pair_or_null = Pair_or_null
module Monitor = Panic.Monitor

module For_scheduler = struct
  module Ivar = Ivar

  let root ~monitor ~promote ~wake ~f = Scheduler.root #{ monitor; promote; wake } ~f
end

module For_testing = struct
  module Runqueue = struct
    include Runqueue
    include Runqueue.For_testing
  end
end

exception Panic = Panic.Panic

include Parallel_kernel1

let panic t incident =
  Panic.Monitor.panic (monitor t) incident;
  raise (Panic (Panic.of_incident incident))
;;

let[@inline] [@loop always] [@tail_mod_cons] rec unwrap_panics
  : type l. l Hlist.Gen(Panic.Result).t @ contended local portable -> Panic.t list
  = function
  | [] -> []
  | Ok _ :: pp -> unwrap_panics pp
  | Panic p :: pp -> p :: unwrap_panics pp
;;

let[@inline never] repanic panics =
  let panics = unwrap_panics panics in
  raise (Panic (Panic.join panics))
;;

let[@inline] uncapsulate a key = Capsule.Data.unwrap ~access:(Capsule.Key.destroy key) a

module Scheduler = struct
  module type S = Parallel_scheduler_intf.S with type parallel := t
  module type S_async = Parallel_scheduler_intf.S_async with type parallel := t

  module Sequential = struct
    type t = { mutable stopped : bool }

    let create () = { stopped = false }

    let stop t =
      if t.stopped then failwith "The scheduler is already stopped";
      t.stopped <- true
    ;;

    let schedule t ~monitor ~f =
      if t.stopped then failwith "The scheduler is already stopped";
      match
        Panic.Result.handle_panics_and_report_exceptions monitor (fun () ->
          f (create_sequential monitor) [@nontail])
      with
      | Ok (a, key) -> uncapsulate a key
      | Panic panic -> raise (Panic panic)
    ;;
  end

  let[@inline] heartbeats () = Atomic.get heartbeat_counter

  let[@inline] check_heartbeat = function
    | Sequential _ -> ()
    | Parallel { monitor; password; queue; _ } ->
      (* May call the scheduler-provided [promote] and [wake] functions,
         which are not allowed to raise. *)
      Unsafe_capsule.Data.iter_local__promise_no_exn
        queue
        ~password
        ~f:(fun [@inline] queue ->
          if queue.heartbeats < Atomic.get heartbeat_counter
          then
            Scheduler.promote
              #{ monitor; promote = queue.promote; wake = queue.wake }
              ~queue)
      [@nontail]
  ;;

  let[@inline never] eager_fork_join t ~continue ~fork ~join =
    (* This always tail-calls either continue or join. Threading [t] through the various
       functions means the global closures don't capture any values, so won't be allocated. *)
    match t with
    | Sequential _ -> continue t [@tail]
    | Parallel { monitor; password; queue; _ } ->
      (match%optional_u.Pair_or_null fork t with
       | None -> continue t [@tail]
       | Some ff ->
         let #(f1, f2) = ff in
         let promote, wake =
           Unsafe_capsule.Data.extract__promise_no_exn queue ~password ~f:(fun queue ->
             queue.promote, queue.wake)
         in
         let one_job = Scheduler.promote_one #{ monitor; promote; wake } ~f:f2 in
         let a = Thunk.apply f1 t in
         let b = Scheduler.One_job.await one_job t in
         (match a, b with
          | Ok (a, keya), Ok (b, keyb) ->
            join t (uncapsulate a keya) (uncapsulate b keyb) [@tail]
          | a, b -> repanic [ a; b ] [@nontail]))
  ;;

  let[@inline] on_heartbeat t ~n ~continue ~fork ~join =
    if Atomic.get heartbeat_counter >= n
    then eager_fork_join t ~continue ~fork ~join
    else continue t
  ;;

  let[@inline] with_jobs t ~queue ~password f ff = exclave_
    (* [Runqueue.with_jobs] does not raise. *)
    Unsafe_capsule.access_local__promise_no_exn ~password ~f:(fun [@inline] access ->
      exclave_
      let queue = Capsule.Data.Local.unwrap ~access queue in
      Runqueue.with_jobs queue f ff t)
    [@nontail]
  ;;
end

let[@inline] [@loop always] [@tail_mod_cons] rec unwrap
  : type l. l Hlist.Gen(Panic.Result).t @ contended local portable unique -> l Hlist.t
  = function
  | [] -> []
  | Ok (a, key) :: aa -> uncapsulate a key :: unwrap aa
  | Panic _ :: _ as l -> (repanic [@tailcall false]) l
;;

let[@inline never] fork_join_seq t ff =
  let[@inline] [@loop always] rec aux
    : type l.
      l Hlist.Gen(Thunk).t @ local once portable
      -> l Hlist.Gen(Panic.Result).t @ local portable unique
    = function
    | [] -> []
    | f :: ff ->
      exclave_
      let f = Thunk.apply f t in
      f :: aux ff
  in
  unwrap (aux ff) [@nontail]
;;

let[@inline] fork_join (type l) t (ff : l Hlist.Gen(Thunk).t) : l Hlist.t =
  match t with
  | Sequential _ -> fork_join_seq t ff
  | Parallel { password; queue; _ } ->
    Scheduler.check_heartbeat t;
    (match ff with
     | [] -> []
     | [ f ] -> unwrap [ Thunk.apply f t ] [@nontail]
     | f :: (_ :: _ as ff) ->
       let first, rest = Scheduler.with_jobs t ~queue ~password f ff in
       unwrap (first :: rest) [@nontail])
;;

let[@inline never] fork_join2_seq t f1 f2 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  match a, b with
  | Ok (a, keya), Ok (b, keyb) -> uncapsulate a keya, uncapsulate b keyb
  | a, b -> repanic [ a; b ] [@nontail]
;;

let[@inline] fork_join2 t f1 f2 =
  match t with
  | Sequential _ -> fork_join2_seq t f1 f2
  | Parallel { password; queue; _ } ->
    Scheduler.check_heartbeat t;
    (match Scheduler.with_jobs t ~queue ~password f1 [ f2 ] with
     | Ok (a, keya), [ Ok (b, keyb) ] -> uncapsulate a keya, uncapsulate b keyb
     | a, b -> repanic (a :: b) [@nontail])
;;

let[@inline never] fork_join3_seq t f1 f2 f3 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  match a, b, c with
  | Ok (a, keya), Ok (b, keyb), Ok (c, keyc) ->
    uncapsulate a keya, uncapsulate b keyb, uncapsulate c keyc
  | a, b, c -> repanic [ a; b; c ] [@nontail]
;;

let[@inline] fork_join3 t f1 f2 f3 =
  match t with
  | Sequential _ -> fork_join3_seq t f1 f2 f3
  | Parallel { password; queue; _ } ->
    Scheduler.check_heartbeat t;
    (match Scheduler.with_jobs t ~queue ~password f1 [ f2; f3 ] with
     | Ok (a, keya), [ Ok (b, keyb); Ok (c, keyc) ] ->
       uncapsulate a keya, uncapsulate b keyb, uncapsulate c keyc
     | a, bc -> repanic (a :: bc) [@nontail])
;;

let[@inline never] fork_join4_seq t f1 f2 f3 f4 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  let d = Thunk.apply f4 t in
  match a, b, c, d with
  | Ok (a, keya), Ok (b, keyb), Ok (c, keyc), Ok (d, keyd) ->
    uncapsulate a keya, uncapsulate b keyb, uncapsulate c keyc, uncapsulate d keyd
  | a, b, c, d -> repanic [ a; b; c; d ] [@nontail]
;;

let[@inline] fork_join4 t f1 f2 f3 f4 =
  match t with
  | Sequential _ -> fork_join4_seq t f1 f2 f3 f4
  | Parallel { password; queue; _ } ->
    Scheduler.check_heartbeat t;
    (match Scheduler.with_jobs t ~queue ~password f1 [ f2; f3; f4 ] with
     | Ok (a, keya), [ Ok (b, keyb); Ok (c, keyc); Ok (d, keyd) ] ->
       uncapsulate a keya, uncapsulate b keyb, uncapsulate c keyc, uncapsulate d keyd
     | a, bcd -> repanic (a :: bcd) [@nontail])
;;

let[@inline never] fork_join5_seq t f1 f2 f3 f4 f5 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  let d = Thunk.apply f4 t in
  let e = Thunk.apply f5 t in
  match a, b, c, d, e with
  | Ok (a, keya), Ok (b, keyb), Ok (c, keyc), Ok (d, keyd), Ok (e, keye) ->
    ( uncapsulate a keya
    , uncapsulate b keyb
    , uncapsulate c keyc
    , uncapsulate d keyd
    , uncapsulate e keye )
  | a, b, c, d, e -> repanic [ a; b; c; d; e ] [@nontail]
;;

let[@inline] fork_join5 t f1 f2 f3 f4 f5 =
  match t with
  | Sequential _ -> fork_join5_seq t f1 f2 f3 f4 f5
  | Parallel { password; queue; _ } ->
    Scheduler.check_heartbeat t;
    (match Scheduler.with_jobs t ~queue ~password f1 [ f2; f3; f4; f5 ] with
     | Ok (a, keya), [ Ok (b, keyb); Ok (c, keyc); Ok (d, keyd); Ok (e, keye) ] ->
       ( uncapsulate a keya
       , uncapsulate b keyb
       , uncapsulate c keyc
       , uncapsulate d keyd
       , uncapsulate e keye )
     | a, bcde -> repanic (a :: bcde) [@nontail])
;;

(* Implemented as a separate function from [fold] for speed. *)
let[@inline] for_ ?(grain = 1) t ~start ~stop ~f =
  let[@inline] [@loop always] rec aux t ~hb ~start ~stop =
    if start >= stop
    then ()
    else
      Scheduler.on_heartbeat
        t
        ~n:hb
        ~continue:(fun [@inline] t ->
          let chunk = Int.min (start + grain) stop in
          for i = start to chunk - 1 do
            f t i
          done;
          aux t ~hb ~start:chunk ~stop)
        ~fork:(fun _ ->
          let chunk = (stop - start) / 2 in
          let pivot = start + chunk in
          if chunk < grain
          then Pair_or_null.none ()
          else
            Pair_or_null.some
              (fun t ->
                let hb = Scheduler.heartbeats () + 1 in
                aux t ~hb ~start ~stop:pivot)
              (fun t ->
                let hb = Scheduler.heartbeats () + 1 in
                aux t ~hb ~start:pivot ~stop))
        ~join:(fun _ () () -> ())
  in
  if grain < 1 then invalid_arg "grain < 1";
  let hb = Scheduler.heartbeats () + 1 in
  match
    Panic.Result.handle_panics_and_report_exceptions (monitor t) (fun [@inline] () ->
      aux t ~hb ~start ~stop)
  with
  | Ok _ -> ()
  | panic -> repanic [ panic ] [@nontail]
;;

let[@inline] fold ?(grain = 1) t ~init:#(zero, state) ~next ~fork ~join =
  let open struct
    type stop =
      | Yield
      | Done
  end in
  let zero = { portended = zero } in
  let state = { portended = state } in
  let[@inline] [@loop always] rec seq t ~n ~state ~acc =
    if n = 0
    then #(Yield, state, acc)
    else (
      match%optional_u.Pair_or_null next t acc.portended state.portended with
      | None -> #(Done, state, acc)
      | Some acc_state ->
        let #(acc, state) = acc_state in
        seq t ~n:(n - 1) ~state:{ portended = state } ~acc:{ portended = acc } [@tail])
  in
  let[@inline] [@loop always] rec aux t ~hb ~state ~acc =
    Scheduler.on_heartbeat
      t
      ~n:hb
      ~continue:(fun [@inline] t ->
        let #(stop, state, acc) = seq t ~n:grain ~state ~acc in
        match stop with
        | Yield -> aux t ~hb ~state ~acc [@tail]
        | Done -> acc)
      ~fork:(fun t ->
        match%optional_u.Pair_or_null fork t state.portended with
        | None -> Pair_or_null.none ()
        | Some s ->
          let #(s0, s1) = s in
          Pair_or_null.some
            (fun t ->
              let hb = Scheduler.heartbeats () + 1 in
              aux t ~hb ~state:{ portended = s0 } ~acc [@tail])
            (fun t ->
              let hb = Scheduler.heartbeats () + 1 in
              aux t ~hb ~state:{ portended = s1 } ~acc:zero [@tail]))
      ~join:(fun [@inline] t s0 s1 -> { portended = join t s0.portended s1.portended })
    [@tail]
  in
  if grain < 1 then invalid_arg "grain < 1";
  let hb = Scheduler.heartbeats () + 1 in
  match
    Panic.Result.handle_panics_and_report_exceptions (monitor t) (fun [@inline] () ->
      aux t ~hb ~acc:zero ~state)
  with
  | Ok (acc, key) -> (uncapsulate acc key).portended
  | panic -> repanic [ panic ] [@nontail]
;;
