open! Base
open! Import
module Hlist = Hlist
module TLS = Domain.Safe.TLS
include Parallel_kernel1

module For_scheduler = struct
  module Result = Result

  let protect ~f ~finally =
    match f () with
    | res ->
      finally ();
      res
    | exception exn ->
      let bt = Backtrace.Exn.most_recent () in
      finally ();
      Exn.raise_with_original_backtrace exn bt
  ;;

  let hook_installed = Atomic.make false

  external setup_hook : unit -> unit @@ portable = "parallel_setup_tick_hook" [@@noalloc]

  let[@inline] acquire () =
    if not (Atomic.get hook_installed)
    then if not (Atomic.exchange hook_installed true) then setup_hook ();
    Domain.Tick.acquire ~interval_usec:Env.heartbeat_interval_us
  ;;

  let[@inline] with_heartbeat f =
    let tick = acquire () in
    protect ~f ~finally:(fun () -> Domain.Tick.release tick)
  ;;

  let heartbeat_mask = TLS.new_key (fun () -> 0)

  let[@inline] without_heartbeat (f @ unyielding) =
    (* [f] is unyielding, so we will not switch threads between acquire and release. *)
    TLS.set heartbeat_mask (TLS.get heartbeat_mask + 1);
    protect ~f ~finally:(fun () -> TLS.set heartbeat_mask (TLS.get heartbeat_mask - 1))
  ;;

  external setup_heartbeat
    :  key:t Stack_pointer.Imm.t Dynamic.t
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

  let () = setup_heartbeat ~key:Dynamic.key ~callback

  let root_exn f ~task ~subtask ~try_wake =
    let (P key) = Capsule.create () in
    Promise.fiber_exn
      (Promise.start ())
      (fun parallel ->
        f parallel;
        exclave_ Ok (Capsule.Data.inject (), key))
      ~scheduler:#{ task; subtask; try_wake }
      ~tokens:0
  ;;

  let await t terminator = exclave_
    match t with
    | Sequential -> (Await.Expert.create [@alloc stack]) ~sync:Sync.blocking ~terminator
    | Parallel _ ->
      let sync #({ portended = t }, { global = trigger }) =
        Parallel_kernel0.Wait.Contended.perform (handler_exn t) (Await trigger) [@nontail]
      in
      let yield t =
        Parallel_kernel0.Wait.Contended.perform (handler_exn t) Yield [@nontail]
      in
      let sync = (Sync.create [@alloc stack]) ~yield:(This yield) ~sync t in
      (Await.Expert.create [@alloc stack]) ~sync ~terminator
  ;;
end

module For_testing = struct
  module Runqueue = struct
    include Runqueue
    include Runqueue.For_testing
  end
end

let sequential = Sequential
let sync _ = Sync.blocking

module Lazy = Await_sync.Expert.Lazy.Make (struct
    type nonrec t = t

    let unsafe_to_await par = exclave_
      (Await.Expert.create [@alloc stack])
        ~sync:(sync par)
        ~terminator:Terminator.unkillable
    ;;
  end)

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
  let { many = { contended = { forkable = first, rest } } } =
    (Capsule.Password.with_current [@mode local])
      current
      (fun [@inline] current -> exclave_
         let[@inline] f (t : t) =
           (Capsule.access ~password:current ~f:(fun [@inline] access ->
              let f = Capsule.Data.Local.unwrap_once ~access f in
              { aliased_many = Capsule.Data.wrap ~access (f t) }))
             .aliased_many
         in
         { many =
             { contended =
                 Capsule.access_local ~password ~f:(fun [@inline] access -> exclave_
                   let queue = Capsule.Data.Local.unwrap ~access queue in
                   { forkable = Runqueue.with_jobs queue f ff t })
             }
         })
  in
  #(Result.map ~f:(Capsule.Data.unwrap ~access:current) first, { contended = rest })
;;

module Seq = struct
  let[@inline never] for_ t ~start ~stop ~f =
    for i = start to stop - 1 do
      f t i
    done
  ;;

  let[@inline never] fork_join t ff =
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

  let[@inline never] fork_join2 t f1 f2 =
    let a = Thunk.apply f1 t in
    let b = Thunk.apply f2 t in
    let [ { global = a }; { global = b } ] = unwrap_local [ a; b ] in
    #(a, b)
  ;;

  let[@inline never] fork_join3 t f1 f2 f3 =
    let a = Thunk.apply f1 t in
    let b = Thunk.apply f2 t in
    let c = Thunk.apply f3 t in
    let [ { global = a }; { global = b }; { global = c } ] = unwrap_local [ a; b; c ] in
    #(a, b, c)
  ;;

  let[@inline never] fork_join4 t f1 f2 f3 f4 =
    let a = Thunk.apply f1 t in
    let b = Thunk.apply f2 t in
    let c = Thunk.apply f3 t in
    let d = Thunk.apply f4 t in
    let [ { global = a }; { global = b }; { global = c }; { global = d } ] =
      unwrap_local [ a; b; c; d ]
    in
    #(a, b, c, d)
  ;;

  let[@inline never] fork_join5 t f1 f2 f3 f4 f5 =
    let a = Thunk.apply f1 t in
    let b = Thunk.apply f2 t in
    let c = Thunk.apply f3 t in
    let d = Thunk.apply f4 t in
    let e = Thunk.apply f5 t in
    let [ { global = a }; { global = b }; { global = c }; { global = d }; { global = e } ]
      =
      unwrap_local [ a; b; c; d; e ]
    in
    #(a, b, c, d, e)
  ;;
end

module Biased = struct
  let[@inline] fork_join2 t f1 f2 =
    match t with
    | Sequential -> Seq.fork_join2 t f1 f2
    | Parallel { queue; password; scheduler; _ } ->
      use_tokens ~queue ~password ~scheduler;
      let #(first, rest) = with_jobs t ~queue ~password f1 [ f2 ] in
      let [ a; b ] = unwrap_encapsulated first rest.contended in
      #(a, b)
  ;;

  let[@inline] fork_join3 t f1 f2 f3 =
    match t with
    | Sequential -> Seq.fork_join3 t f1 f2 f3
    | Parallel { queue; password; scheduler; _ } ->
      use_tokens ~queue ~password ~scheduler;
      let #(first, rest) = with_jobs t ~queue ~password f1 [ f2; f3 ] in
      let [ a; b; c ] = unwrap_encapsulated first rest.contended in
      #(a, b, c)
  ;;

  let[@inline] fork_join4 t f1 f2 f3 f4 =
    match t with
    | Sequential -> Seq.fork_join4 t f1 f2 f3 f4
    | Parallel { queue; password; scheduler; _ } ->
      use_tokens ~queue ~password ~scheduler;
      let #(first, rest) = with_jobs t ~queue ~password f1 [ f2; f3; f4 ] in
      let [ a; b; c; d ] = unwrap_encapsulated first rest.contended in
      #(a, b, c, d)
  ;;

  let[@inline] fork_join5 t f1 f2 f3 f4 f5 =
    match t with
    | Sequential -> Seq.fork_join5 t f1 f2 f3 f4 f5
    | Parallel { queue; password; scheduler; _ } ->
      use_tokens ~queue ~password ~scheduler;
      let #(first, rest) = with_jobs t ~queue ~password f1 [ f2; f3; f4; f5 ] in
      let [ a; b; c; d; e ] = unwrap_encapsulated first rest.contended in
      #(a, b, c, d, e)
  ;;
end

(* The following magic is sound because:
   - All tasks are [forkable shareable], so may read the environment but not mutate it.
   - Our caller is blocked until their completion, so cannot mutate the environment.
   - None of the tasks escape their scope. *)
module Magic = struct
  external require_forkable_shareable
    :  ('a[@local_opt]) @ forkable once shareable
    -> ('a[@local_opt]) @ forkable once portable
    @@ portable
    = "%identity"
end

let[@inline] fork_join (type l) t (ff : l Hlist.Gen(Thunk).t) : l Hlist.t =
  match t with
  | Sequential -> Seq.fork_join t ff
  | Parallel { queue; password; scheduler; _ } ->
    use_tokens ~queue ~password ~scheduler;
    (match Magic.require_forkable_shareable ff with
     | [] -> []
     | [ f ] -> unwrap [ Thunk.apply f t ] [@nontail]
     | f :: (_ :: _ as ff) ->
       let #(first, rest) = with_jobs t ~queue ~password f ff in
       unwrap_encapsulated first rest.contended [@nontail])
;;

let[@inline] fork_join2 t f1 f2 =
  Biased.fork_join2
    t
    (Magic.require_forkable_shareable f1)
    (Magic.require_forkable_shareable f2) [@nontail]
;;

let[@inline] fork_join3 t f1 f2 f3 =
  Biased.fork_join3
    t
    (Magic.require_forkable_shareable f1)
    (Magic.require_forkable_shareable f2)
    (Magic.require_forkable_shareable f3) [@nontail]
;;

let[@inline] fork_join4 t f1 f2 f3 f4 =
  Biased.fork_join4
    t
    (Magic.require_forkable_shareable f1)
    (Magic.require_forkable_shareable f2)
    (Magic.require_forkable_shareable f3)
    (Magic.require_forkable_shareable f4) [@nontail]
;;

let[@inline] fork_join5 t f1 f2 f3 f4 f5 =
  Biased.fork_join5
    t
    (Magic.require_forkable_shareable f1)
    (Magic.require_forkable_shareable f2)
    (Magic.require_forkable_shareable f3)
    (Magic.require_forkable_shareable f4)
    (Magic.require_forkable_shareable f5) [@nontail]
;;

let[@inline] for_ t ~start ~stop ~f =
  let[@inline] rec aux t ~start ~stop ~grain =
    if start >= stop
    then ()
    else (
      (* We compute [grain] sequential iterations while forking off the rest of the loop.
         If the fork is not stolen, it continues iterating sequentially with twice the
         granularity. Otherwise, the fork splits the remaining work and resets [grain].
         This strategy aggregates cheap iterations into sequential blocks while avoiding
         blocking on expensive iterations. *)
      let pivot = Int.min (start + grain) stop in
      Stack_pointer.unsafe_with_value t ~f:(fun [@inline] t_ptr ->
        let #((), ()) =
          fork_join2
            t
            (fun [@inline] t ->
              for i = start to pivot - 1 do
                f t i
              done)
            (fun [@inline] t ->
              (* We get a physically distinct [t] iff we were stolen. *)
              if Stack_pointer.unsafe_with_value t ~f:(fun [@inline] t_ptr' ->
                   Stack_pointer.equal t_ptr t_ptr')
              then aux t ~start:pivot ~stop ~grain:(grain lsl 1)
              else
                (* [fork] resets [grain], so worst-case stack depth is θ(log^2(n)). *)
                fork t ~start:pivot ~stop)
        in
        ())
      [@nontail])
  and[@cold] fork t ~start ~stop =
    if start >= stop
    then ()
    else (
      let chunk = (stop - start) / 2 in
      let pivot = start + chunk in
      let #((), ()) =
        fork_join2
          t
          (fun [@inline] t -> aux t ~start ~stop:pivot ~grain:1)
          (fun [@inline] t -> aux t ~start:pivot ~stop ~grain:1)
      in
      ())
  in
  match t with
  | Sequential -> Seq.for_ t ~start ~stop ~f
  | Parallel _ -> aux t ~start ~stop ~grain:1
;;

let%template[@inline] fold
  (type (acc : acc) (seq : seq mod shareable shared))
  t
  ~init
  ~state
  ~next
  ~stop
  ~fork
  ~join
  =
  let open struct
    type yield =
      | Yield
      | Done
  end in
  let[@inline] [@loop] rec seq t ~n ~state ~acc =
    if n = 0
    then #(Yield, state, acc)
    else (
      match (next t acc state : (#(acc * seq) Option_u.t[@kind acc & seq])) with
      | T #(None, _) -> #(Done, state, acc)
      | T #(Some, #(acc, state)) -> seq t ~n:(n - 1) ~state ~acc)
  in
  let[@inline] [@loop] rec aux t ~state ~acc ~grain =
    if has_tokens t
    then (
      match (fork t state : (#(seq * seq) Option_u.t[@kind seq & seq])) with
      | T #(None, _) ->
        let #(_, _, acc) = seq t ~n:Int.max_value ~state ~acc in
        stop t acc
      | T #(Some, #(s0, s1)) ->
        let #(a, b) =
          fork_join2
            t
            (fun t -> aux t ~state:s0 ~acc:(init ()) ~grain:1)
            (fun t -> aux t ~state:s1 ~acc:(init ()) ~grain:1)
        in
        join t (stop t acc) (join t a b))
    else (
      let #(yield, state, acc) = seq t ~n:grain ~state ~acc in
      match yield with
      | Done -> stop t acc
      | Yield -> aux t ~acc ~state ~grain:(grain lsl 1))
  in
  aux t ~acc:(init ()) ~state ~grain:1
[@@kind
  acc = (base_or_null, value_or_null & base_or_null)
  , seq = (base_or_null, value_or_null & value_or_null)]
;;
