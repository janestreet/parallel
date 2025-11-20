open! Base
open! Import
module Hlist = Hlist
module TLS = Domain.Safe.TLS
module Pair_or_null = Pair_or_null
include Parallel_kernel1

module For_scheduler = struct
  module Result = Result

  exception Out_of_fibers = Promise.Out_of_fibers

  external acquire : unit -> unit @@ portable = "parallel_acquire_heartbeat" [@@noalloc]
  external release : unit -> unit @@ portable = "parallel_release_heartbeat" [@@noalloc]

  let[@inline] with_heartbeat f =
    acquire ();
    Exn.protect ~f ~finally:release
  ;;

  let heartbeat_mask = TLS.new_key (fun () -> 0)

  let[@inline] without_heartbeat (f @ unyielding) =
    (* [f] is unyielding, so we will not switch threads between acquire and release. *)
    TLS.set heartbeat_mask (TLS.get heartbeat_mask + 1);
    Exn.protectx ~f () ~finally:(fun () ->
      TLS.set heartbeat_mask (TLS.get heartbeat_mask - 1))
  ;;

  external setup_heartbeat
    :  interval_us:int
    -> key:t Stack_pointer.Imm.t Dynamic.t
    -> callback:(t Stack_pointer.Imm.t @ local -> unit)
    -> unit
    = "parallel_setup_heartbeat"

  let[@inline] promote queue ~scheduler =
    without_heartbeat (fun () -> Runqueue.promote queue ~scheduler) [@nontail]
  ;;

  let callback queue =
    let queue = Stack_pointer.Imm.to_ptr queue in
    Stack_pointer.use queue ~f:(function [@inline]
      | None | Some Sequential -> ()
      | Some (Parallel { password; queue; scheduler; _ }) ->
        Capsule.Data.Local.iter queue ~password ~f:(fun [@inline] queue ->
          Runqueue.add_tokens queue Env.heartbeat_promotions;
          if TLS.get heartbeat_mask = 0 then promote queue ~scheduler)
        [@nontail])
      [@nontail]
  ;;

  let () =
    setup_heartbeat ~interval_us:Env.heartbeat_interval_us ~key:Dynamic.key ~callback
  ;;

  let root_exn f ~promote ~wake =
    let (P key) = Capsule.create () in
    Promise.fiber_exn
      (Promise.start ())
      (fun parallel ->
        f parallel;
        exclave_ Ok (Capsule.Data.inject (), key))
      ~scheduler:#{ promote; wake }
      ~tokens:0
  ;;

  let[@inline] await parallel trigger =
    Parallel_kernel0.Wait.Contended.perform
      (Parallel_kernel1.handler_exn parallel)
      (Trigger trigger) [@nontail]
  ;;
end

module For_testing = struct
  module Runqueue = struct
    include Runqueue
    include Runqueue.For_testing
  end
end

let[@inline] [@loop] [@unroll] [@tail_mod_cons] rec unwrap
  : type l. l Hlist.Gen(Result).t @ local -> l Hlist.t
  = function
  | [] -> []
  | a :: aa -> Result.ok_exn a :: unwrap aa
;;

let[@inline] [@loop] [@unroll] rec unwrap_local
  : type l. l Hlist.Gen(Result).t @ local -> l Hlist.Gen(Modes.Global).t @ local
  =
  function%exclave
  | [] -> []
  | a :: aa ->
    let a =
      (* NB: need to evaluate this before constructing the result list so we keep the
         "raise the leftmost exception" behavior of [unwrap] *)
      Result.ok_exn a
    in
    { global = a } :: unwrap_local aa
;;

let[@inline] unwrap_encapsulated (first : _ Result.t) rest : _ Hlist.t =
  let[@inline] [@loop] [@unroll] [@tail_mod_cons] rec unwrap_tail
    : type l. l Hlist.Gen(Result.Capsule).t @ contended local unique -> l Hlist.t
    = function
    | [] -> []
    | a :: aa -> Result.Capsule.unwrap_ok_exn a :: unwrap_tail aa
  in
  Result.ok_exn first :: unwrap_tail rest
;;

module Scheduler = struct
  module type S = Parallel_scheduler_intf.S with type parallel := t
  module type S_concurrent = Parallel_scheduler_intf.S_concurrent with type parallel := t

  module Sequential = struct
    type t = { mutable stopped : bool }

    let create ?max_domains:_ () = { stopped = false }
    let is_stopped t = t.stopped

    let stop t =
      if t.stopped then failwith "The scheduler is already stopped";
      t.stopped <- true
    ;;

    let parallel t ~f =
      if t.stopped then failwith "The scheduler is already stopped";
      f Sequential
    ;;
  end

  let[@inline] use_tokens ~queue ~password ~scheduler =
    Capsule.Data.Local.iter queue ~password ~f:(fun [@inline] (queue : Runqueue.t) ->
      if queue.tokens > 0 then For_scheduler.promote queue ~scheduler)
    [@nontail]
  ;;

  let[@inline] has_tokens = function
    | Sequential -> false
    | Parallel { queue; password; _ } ->
      Capsule.Data.Local.extract queue ~password ~f:(fun [@inline] (queue : Runqueue.t) ->
        queue.tokens > 0)
      [@nontail]
  ;;

  let[@inline] heartbeat t ~n =
    match t with
    | Sequential -> ()
    | Parallel { queue; password; scheduler; _ } ->
      Capsule.Data.Local.iter queue ~password ~f:(fun [@inline] queue ->
        Runqueue.add_tokens queue n;
        if queue.tokens > 0 then For_scheduler.promote queue ~scheduler)
      [@nontail]
  ;;

  let[@inline] with_jobs t ~queue ~password f ff = exclave_
    let (P current) = Capsule.current () in
    let f = Capsule.Data.Local.wrap_once ~access:current f in
    let { contended = { forkable = first, rest } } =
      Capsule.Password.with_current current (fun [@inline] current -> exclave_
        let[@inline] f (t : t) =
          Capsule.access ~password:current ~f:(fun [@inline] access ->
            let f = Capsule.Data.Local.unwrap_once ~access f in
            Capsule.Data.wrap ~access (f t))
          [@nontail]
        in
        { contended =
            Capsule.access_local ~password ~f:(fun [@inline] access -> exclave_
              let queue = Capsule.Data.Local.unwrap ~access queue in
              { forkable = Runqueue.with_jobs queue f ff t })
        })
    in
    #(Result.map ~f:(Capsule.Data.unwrap ~access:current) first, { contended = rest })
  ;;
end

let[@inline never] fork_join_seq t ff =
  let[@inline] [@loop] rec aux
    : type l. l Hlist.Gen(Thunk).t @ local once -> l Hlist.Gen(Result).t @ local unique
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
  | Sequential -> fork_join_seq t ff
  | Parallel { queue; password; scheduler; _ } ->
    Scheduler.use_tokens ~queue ~password ~scheduler;
    (match ff with
     | [] -> []
     | [ f ] -> unwrap [ Thunk.apply f t ] [@nontail]
     | f :: (_ :: _ as ff) ->
       let #(first, rest) = Scheduler.with_jobs t ~queue ~password f ff in
       unwrap_encapsulated first rest.contended [@nontail])
;;

let[@inline never] fork_join2_seq t f1 f2 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let [ { global = a }; { global = b } ] = unwrap_local [ a; b ] in
  #(a, b)
;;

let[@inline] fork_join2 t f1 f2 =
  match t with
  | Sequential -> fork_join2_seq t f1 f2
  | Parallel { queue; password; scheduler; _ } ->
    Scheduler.use_tokens ~queue ~password ~scheduler;
    let #(first, rest) = Scheduler.with_jobs t ~queue ~password f1 [ f2 ] in
    let [ a; b ] = unwrap_encapsulated first rest.contended in
    #(a, b)
;;

let[@inline never] fork_join3_seq t f1 f2 f3 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  let [ { global = a }; { global = b }; { global = c } ] = unwrap_local [ a; b; c ] in
  #(a, b, c)
;;

let[@inline] fork_join3 t f1 f2 f3 =
  match t with
  | Sequential -> fork_join3_seq t f1 f2 f3
  | Parallel { queue; password; scheduler; _ } ->
    Scheduler.use_tokens ~queue ~password ~scheduler;
    let #(first, rest) = Scheduler.with_jobs t ~queue ~password f1 [ f2; f3 ] in
    let [ a; b; c ] = unwrap_encapsulated first rest.contended in
    #(a, b, c)
;;

let[@inline never] fork_join4_seq t f1 f2 f3 f4 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  let d = Thunk.apply f4 t in
  let [ { global = a }; { global = b }; { global = c }; { global = d } ] =
    unwrap_local [ a; b; c; d ]
  in
  #(a, b, c, d)
;;

let[@inline] fork_join4 t f1 f2 f3 f4 =
  match t with
  | Sequential -> fork_join4_seq t f1 f2 f3 f4
  | Parallel { queue; password; scheduler; _ } ->
    Scheduler.use_tokens ~queue ~password ~scheduler;
    let #(first, rest) = Scheduler.with_jobs t ~queue ~password f1 [ f2; f3; f4 ] in
    let [ a; b; c; d ] = unwrap_encapsulated first rest.contended in
    #(a, b, c, d)
;;

let[@inline never] fork_join5_seq t f1 f2 f3 f4 f5 =
  let a = Thunk.apply f1 t in
  let b = Thunk.apply f2 t in
  let c = Thunk.apply f3 t in
  let d = Thunk.apply f4 t in
  let e = Thunk.apply f5 t in
  let [ { global = a }; { global = b }; { global = c }; { global = d }; { global = e } ] =
    unwrap_local [ a; b; c; d; e ]
  in
  #(a, b, c, d, e)
;;

let[@inline] fork_join5 t f1 f2 f3 f4 f5 =
  match t with
  | Sequential -> fork_join5_seq t f1 f2 f3 f4 f5
  | Parallel { queue; password; scheduler; _ } ->
    Scheduler.use_tokens ~queue ~password ~scheduler;
    let #(first, rest) = Scheduler.with_jobs t ~queue ~password f1 [ f2; f3; f4; f5 ] in
    let [ a; b; c; d; e ] = unwrap_encapsulated first rest.contended in
    #(a, b, c, d, e)
;;

(* This always tail-calls either continue or join. Threading [t] through the various
   functions means the global closures don't capture any values, so won't be allocated. *)
let[@inline] fork_on_heartbeat t ~grain ~continue ~fork ~join =
  let[@inline never] fork_join t ~continue ~fork ~join =
    match%optional_u.Pair_or_null fork t with
    | Some ff ->
      let #(f1, f2) = ff in
      let #(a, b) = fork_join2 t f1 f2.portable in
      join t a b
    | None -> continue t ~grain
  in
  if Scheduler.has_tokens t then fork_join t ~continue ~fork ~join else continue t ~grain
;;

(* Implemented as a separate function from [fold] for speed. *)
let[@inline] for_ t ~start ~stop ~f =
  (* [grain] is the number of sequential iterations between heartbeat checks. It increases
     geometrically until a heartbeat occurs. *)
  let[@inline] [@loop] rec aux t ~start ~stop ~grain =
    if start >= stop
    then ()
    else
      fork_on_heartbeat
        t
        ~grain
        ~continue:(fun [@inline] t ~grain ->
          let chunk = Int.min (start + grain) stop in
          for i = start to chunk - 1 do
            f t i
          done;
          aux t ~start:chunk ~stop ~grain:(grain lsl 1))
        ~fork:(fun _ ->
          let chunk = (stop - start) / 2 in
          let pivot = start + chunk in
          if chunk < 1
          then Pair_or_null.none ()
          else
            Pair_or_null.some
              (fun t -> aux t ~start ~stop:pivot ~grain:1)
              { portable = (fun t -> aux t ~start:pivot ~stop ~grain:1) })
        ~join:(fun _ () () -> ())
  in
  aux t ~start ~stop ~grain:1
;;

let[@inline] fold
  : ('acc : value mod portable) ('seq : value mod contended portable) 'ret.
  t @ local
  -> init:(unit -> 'acc) @ portable
  -> state:'seq
  -> next:(t @ local -> 'acc -> 'seq -> ('acc, 'seq) Pair_or_null.t) @ portable
  -> stop:(t @ local -> 'acc -> 'ret) @ portable
  -> fork:(t @ local -> 'seq -> ('seq, 'seq) Pair_or_null.t) @ portable
  -> join:(t @ local -> 'ret -> 'ret -> 'ret) @ portable
  -> 'ret
  =
  fun t ~init ~state ~next ~stop ~fork ~join ->
  let open struct
    type yield =
      | Yield
      | Done
  end in
  let[@inline] [@loop] rec seq t ~n ~state ~acc =
    if n = 0
    then #(Yield, state, acc)
    else (
      match%optional_u.Pair_or_null next t acc state with
      | None -> #(Done, state, acc)
      | Some acc_state ->
        let #(acc, state) = acc_state in
        seq t ~n:(n - 1) ~state ~acc)
  in
  let[@inline] [@loop] rec aux t ~state ~acc ~grain =
    fork_on_heartbeat
      t
      ~grain
      ~continue:(fun [@inline] t ~grain ->
        let #(yield, state, acc) = seq t ~n:grain ~state ~acc in
        match yield with
        | Yield -> aux t ~state ~acc ~grain:(grain lsl 1)
        | Done -> stop t acc)
      ~fork:(fun t ->
        match%optional_u.Pair_or_null fork t state with
        | None -> Pair_or_null.none ()
        | Some s ->
          let #(s0, s1) = s in
          Pair_or_null.some
            (fun t -> aux t ~state:s0 ~acc ~grain:1)
            { portable = (fun t -> aux t ~state:s1 ~acc:(init ()) ~grain:1) })
      ~join
  in
  aux t ~acc:(init ()) ~state ~grain:1
;;
