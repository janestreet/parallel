open! Base
open! Import
module Wait = Parallel_kernel0.Wait
include Parallel_kernel0.Promise

(** Promises must be applied exactly once and awaited exactly once.

    Allowed state transitions:
    - Start -> Claimed
    - Claimed -> Blocking
    - Claimed -> Ready
    - Blocking -> Ready
    - Ready -> Blocking

    {v
  +-----------+------------+------------+-------------+--------------+
  | Action    | Old State  | New State  | Final State | Result       |
  +-----------+------------+------------+-------------+--------------+
  | Create    |            | Start      | Start       |              |
  | Apply f   | Start      | Claimed    | Claimed     | Fill (f ())  |
  | Apply f   | Claimed    | Claimed    | Claimed     |              |
  | Await f   | Start      | Claimed    | Claimed     | f ()         |
  | Await f   | Claimed    | Claimed    | Claimed     | Suspend cc   |
  | Await f   | Ready a    | Ready a    | Claimed     | a            |
  | Fill a    | Claimed    | Ready a    | Ready a     |              |
  | Fill a    | Blocking k | Ready a    | Claimed     | promote k a  |
  | Suspend k | Claimed    | Blocking k | Blocking k  |              |
  | Suspend k | Ready a    | Blocking k | Claimed     | continue k a |
  +-----------+------------+------------+--+-------------------------+
    v} *)

type 'k suspension =
  | Done
  | Trigger of Await.Trigger.t @@ aliased many * (unit continuation, 'k) Capsule.Data.t
  | Promise :
      'a t @@ aliased many * ('a Result.Capsule.t continuation, 'k) Capsule.Data.t
      -> 'k suspension

let[@inline] start () = Unique.Atomic.make Start

let[@inline] [@loop] rec continue
  : type (a : value mod contended) k.
    a @ portable unique
    -> scheduler:Parallel_kernel0.Scheduler.t
    -> key:k Capsule.Key.t @ unique
    -> cont:
         ( (a, (unit, unit) Wait.Contended.Result.t, unit) Handled_effect.Continuation.t
           , k )
           Capsule.Data.t
       @ unique
    -> unit
  =
  fun a ~scheduler ~key ~cont ->
  let #(result, key) =
    Capsule.Key.access key ~f:(fun [@inline] access ->
      let cont = Capsule.Data.unwrap_unique ~access cont in
      match Handled_effect.continue cont a [] with
      | Value () -> Done
      | Exception exn ->
        (* Cannot have come from the job; indicates a scheduler bug *)
        raise exn
      | Operation (Promise t, cont) -> Promise (t, Capsule.Data.wrap_unique ~access cont)
      | Operation (Trigger t, cont) -> Trigger (t, Capsule.Data.wrap_unique ~access cont))
  in
  let key = Capsule.Key.globalize_unique key in
  match result with
  | Done -> ()
  | Trigger (t, cont) ->
    let[@inline] continue () = continue () ~scheduler ~key ~cont in
    (match
       (* Awaited triggers cannot be dropped, so we don't need to [discontinue]. *)
       Await.Trigger.on_signal
         t
         ~f:(fun continue ->
           scheduler.#promote continue;
           scheduler.#wake ~n:1)
         continue
     with
     | Null -> ()
     | This continue -> continue ())
  | Promise (t, cont) ->
    (match Unique.Atomic.exchange t (Blocking { key; cont }) with
     | Claimed -> ()
     | Ready a ->
       (match Unique.Atomic.exchange t Claimed with
        | Blocking { key; cont } -> continue a ~scheduler ~key ~cont
        | Start | Claimed | Ready _ ->
          (* Impossible: the promise has been [fill]ed, so we are the only writer, and we
             just wrote [Blocking]. *)
          assert false)
     | Start | Blocking _ ->
       (* Impossible: unclaimed jobs are never [await]ed, and claimed jobs are [await]ed
          exactly once. *)
       assert false)
;;

let[@inline] fill t a ~(scheduler : Parallel_kernel0.Scheduler.t) =
  match Unique.Atomic.exchange t (Ready a) with
  | Claimed -> ()
  | Blocking { key; cont } ->
    (match Unique.Atomic.exchange t Claimed with
     | Ready a ->
       scheduler.#promote (fun () -> continue a ~scheduler ~key ~cont)
       (* We do not call [scheduler.#wake], as this worker is about to return to the
          scheduler. *)
     | Start | Claimed | Blocking _ ->
       (* Impossible: the promise has been [await]ed, so only we are the only writer, and
          we just wrote [Ready]. *)
       assert false)
  | Start | Ready _ ->
    (* Impossible: we claimed the job, and claimed jobs are [fill]ed exactly once. *)
    assert false
;;

let[@inline] await_or_run t job parallel = exclave_
  match Unique.Atomic.compare_and_set t ~if_phys_equal_to:Start ~replace_with:Claimed with
  | Set_here -> job parallel
  | Compare_failed ->
    (match Unique.Atomic.exchange t Claimed with
     | Claimed ->
       Wait.Contended.perform
         (Parallel_kernel1.handler_exn parallel)
         (Promise t) [@nontail]
     | Ready a -> a
     | Start | Blocking _ ->
       (* Impossible: the job is already claimed, and claimed jobs are [await]ed exactly
          once. *)
       assert false)
;;

let[@inline] apply t job ~scheduler ~tokens ~handler =
  match Unique.Atomic.compare_and_set t ~if_phys_equal_to:Start ~replace_with:Claimed with
  | Set_here ->
    let (P (type k) (key : k Capsule.Key.t)) = Capsule.create () in
    let #((), (_ : k Capsule.Key.t)) =
      Capsule.Key.with_password key ~f:(fun [@inline] password ->
        let result =
          Parallel_kernel1.with_parallel job ~scheduler ~tokens ~password ~handler
        in
        fill t (Result.Capsule.globalize result) ~scheduler)
    in
    ()
  | Compare_failed -> ()
;;

let[@inline] create_fiber t job ~scheduler ~tokens ~key =
  let #({ many = cont }, key) =
    Capsule.Key.access key ~f:(fun [@inline] access ->
      let k =
        (Wait.Contended.fiber [@alert "-experimental_runtime5"]) (fun handler () ->
          apply t job ~scheduler ~tokens ~handler)
      in
      { many = Capsule.Data.wrap_unique ~access k })
  in
  #(cont, key)
;;

let fiber_exn t job ~scheduler ~tokens ~lazy_ =
  let (P key) = Capsule.create () in
  if lazy_
  then
    fun () ->
    let #(cont, key) = create_fiber t job ~scheduler ~tokens ~key in
    continue () ~scheduler ~key ~cont
  else (
    let #(cont, key) = create_fiber t job ~scheduler ~tokens ~key in
    fun () -> continue () ~scheduler ~key ~cont)
;;

let try_fiber t job ~scheduler ~tokens () =
  let (P key) = Capsule.create () in
  (* If we fail to allocate a fiber, we drop the job without claiming the promise. *)
  match create_fiber t job ~scheduler ~tokens ~key with
  | #(cont, key) -> continue () ~scheduler ~key ~cont
  | exception Out_of_fibers -> ()
;;
