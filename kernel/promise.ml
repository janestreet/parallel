open! Base
open! Import
module Await = Parallel_kernel0.Await
include Parallel_kernel0.Promise

(** Promises must be applied exactly once and awaited exactly once.

    Allowed state transitions:
    - Start -> Claimed
    - Claimed -> Blocking
    - Claimed -> Ready
    - Blocking -> Ready
    - Ready -> Blocking

    {v
  +----------+------------+------------+-------------+---------------+
  | Action   | Old State  | New State  | Final State | Result        |
  +----------+------------+------------+-------------+---------------+
  | Create   |            | Start f    | Start f     |               |
  | Apply    | Start f    | Claimed    | Claimed     | Fill (f ())   |
  | Apply    | Claimed    | Claimed    | Claimed     |               |
  | Await    | Start f    | Claimed    | Claimed     | return (f ()) |
  | Await    | Claimed    | Claimed    | Claimed     | Suspend       |
  | Await    | Ready a    | Ready a    | Claimed     | return a      |
  | Fill     | Claimed    | Ready a    | Ready a     |               |
  | Fill     | Blocking k | Ready a    | Claimed     | continue k a  |
  | Suspend  | Claimed    | Blocking k | Blocking k  |               |
  | Suspend  | Ready a    | Blocking k | Claimed     | continue k a  |
  +----------+------------+------------+--+--------------------------+
    v} *)

type 'k suspension : value mod portable =
  | Done : _ suspension
  | Suspended :
      'a t @@ aliased global * ('a continuation, 'k) Capsule.Data.t @@ global
      -> 'k suspension
[@@unsafe_allow_any_mode_crossing]

let[@inline] start () = Unique.Atomic.make Start

let[@inline] [@loop always] rec continue
  : type (a : value mod contended) k.
    a @ portable unique
    -> key:k Capsule.Key.t @ unique
    -> cont:
         ( (a portable, (unit, unit) Await.Contended.Result.t, unit) Effect.Continuation.t
           , k )
           Capsule.Data.t
       @ unique
    -> unit
  =
  fun a ~key ~cont ->
  let result, key =
    Capsule.Key.access_local key ~f:(fun [@inline] access ->
      exclave_
      let cont = Capsule.Data.unwrap_unique ~access cont in
      let res =
        match Effect.continue cont { portable = a } [] with
        | Value () -> Done
        | Exception exn ->
          (* Cannot have come from the job; indicates a scheduler bug *)
          raise exn
        | Operation (Await t, cont) -> Suspended (t, Capsule.Data.wrap_unique ~access cont)
      in
      { many = res })
  in
  match result.many with
  | Done -> ()
  | Suspended (t, cont) ->
    let key = Capsule.Key.globalize_unique key in
    (match Unique.Atomic.exchange t (Blocking { key; cont }) with
     | Claimed -> ()
     | Ready a ->
       (match Unique.Atomic.exchange t Claimed with
        | Blocking { key; cont } -> continue a ~key ~cont [@tail]
        | Start | Claimed | Ready _ ->
          (* Impossible: the promise has been [fill]ed, so we are the only writer, and we just wrote [Blocking]. *)
          assert false)
     | Start | Blocking _ ->
       (* Impossible: unclaimed jobs are never [await]ed, and claimed jobs are [await]ed exactly once. *)
       assert false)
;;

let[@inline] fill t a ~(scheduler : Parallel_kernel0.Scheduler.t) =
  match Unique.Atomic.exchange t (Ready a) with
  | Claimed -> ()
  | Blocking { key; cont } ->
    (match Unique.Atomic.exchange t Claimed with
     | Ready a ->
       scheduler.#promote (fun () -> continue a ~key ~cont)
       (* We do not call [scheduler.#wake], as this worker is about to return to the scheduler. *)
     | Start | Claimed | Blocking _ ->
       (* Impossible: the promise has been [await]ed, so only we are the only writer, and we just wrote [Ready]. *)
       assert false)
  | Start | Ready _ ->
    (* Impossible: we claimed the job, and claimed jobs are [fill]ed exactly once. *)
    assert false
;;

let[@inline] await t job parallel = exclave_
  match Unique.Atomic.compare_and_set t ~if_phys_equal_to:Start ~replace_with:Claimed with
  | Set_here -> job parallel
  | Compare_failed ->
    (match Unique.Atomic.exchange t Claimed with
     | Claimed ->
       Await.Contended.perform
         (Parallel_kernel1.handler_exn parallel)
         (Await t) [@nontail]
     | Ready a -> a
     | Start | Blocking _ ->
       (* Impossible: the job is already claimed, and claimed jobs are [await]ed exactly once. *)
       assert false)
;;

let[@inline] apply t job ~scheduler ~handler =
  match Unique.Atomic.compare_and_set t ~if_phys_equal_to:Start ~replace_with:Claimed with
  | Set_here ->
    let (P (type k) (key : k Capsule.Key.t)) = Capsule.create () in
    let (), (_ : k Capsule.Key.t) =
      Capsule.Key.with_password key ~f:(fun [@inline] password ->
        let result =
          job (Parallel_kernel1.create_parallel ~scheduler ~password ~handler)
        in
        fill t (Panic.Result.globalize result) ~scheduler)
    in
    ()
  | Compare_failed -> ()
;;

let[@inline] fiber t job ~scheduler () =
  let (P key) = Capsule.create () in
  let { many = cont }, key =
    Capsule.Key.access key ~f:(fun [@inline] access ->
      let k =
        (Await.Contended.fiber [@alert "-experimental"]) (fun handler { portable = () } ->
          apply t job ~scheduler ~handler)
      in
      { many = Capsule.Data.wrap_unique ~access k })
  in
  continue () ~key ~cont [@tail]
;;
