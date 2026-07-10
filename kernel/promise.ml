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

(* See [Parallel_kernel0.Ops.t] *)
type 'k suspension =
  | Done
  | Promise :
      'a t @@ aliased many
      * (('a Result.Capsule.t * tokens:int) continuation, 'k) Capsule.Data.t
      -> 'k suspension
  | Await of Trigger.t @@ aliased many * (unit continuation, 'k) Capsule.Data.t
  | Yield of (unit continuation, 'k) Capsule.Data.t

let[@inline] start () = Unique.Atomic.make Start

let[@inline] [@loop] rec continue
  : type (a : value mod contended) k.
    a @ portable unique
    -> scheduler:Parallel_kernel0.Scheduler.t
    -> key:k Capsule.Key.t @ unique
    -> cont:(a continuation, k) Capsule.Data.t @ unique
    -> unit
  =
  fun a ~scheduler ~key ~cont ->
  let #(result, key) =
    Capsule.Key.access key ~f:(fun [@inline] access ->
      let cont = Capsule.Data.unwrap_unique ~access cont in
      match Handled_effect.continue cont a [] with
      | Exception exn ->
        (* Cannot have come from the job; indicates a scheduler bug *)
        raise exn
      | Value () -> Done
      | Operation (Promise t, cont) -> Promise (t, Capsule.Data.wrap_unique ~access cont)
      | Operation (Await t, cont) -> Await (t, Capsule.Data.wrap_unique ~access cont)
      | Operation (Yield, cont) -> Yield (Capsule.Data.wrap_unique ~access cont))
  in
  let key = Capsule.Key.globalize_unique key in
  match result with
  | Done -> ()
  | Promise (t, cont) ->
    (match Unique.Atomic.exchange t (Blocking { key; cont }) with
     | Claimed -> ()
     | Ready { result; tokens } ->
       (match Unique.Atomic.exchange t Claimed with
        | Blocking { key; cont } ->
          continue ((result, ~tokens) : _ * tokens:int) ~scheduler ~key ~cont
        | Start | Claimed | Ready _ ->
          (* Impossible: the promise has been [fill]ed, so we are the only writer, and we
             just wrote [Blocking]. *)
          assert false)
     | Start | Blocking _ ->
       (* Impossible: unclaimed jobs are never [await]ed, and claimed jobs are [await]ed
          exactly once. *)
       assert false)
  | Await (t, cont) ->
    (* Concurrent tasks return to the global queue. *)
    let continue () = continue () ~scheduler ~key ~cont in
    (match
       (* Awaited triggers cannot be dropped, so we don't need to [discontinue]. *)
       Trigger.on_signal t ~f:[%eta1 scheduler.#task] continue
     with
     | Null -> ()
     | This continue -> continue ())
  | Yield cont ->
    (* Concurrent tasks return to the global queue. Yielding is explicitly a request for
       fairness, so this is required. *)
    let continue () = continue () ~scheduler ~key ~cont in
    scheduler.#task continue
;;

let[@inline] fill t a ~(scheduler : Parallel_kernel0.Scheduler.t) ~tokens =
  match Unique.Atomic.exchange t (Ready { result = a; tokens }) with
  | Claimed -> ()
  | Blocking { key; cont } ->
    (match Unique.Atomic.exchange t Claimed with
     | Ready { result; tokens } ->
       scheduler.#subtask (fun () ->
         continue ((result, ~tokens) : _ * tokens:int) ~scheduler ~key ~cont)
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
  | Set_here ->
    (* If the job was not stolen, we don't reclaim its tokens. *)
    #(job parallel, ~tokens:0)
  | Compare_failed ->
    (match Unique.Atomic.exchange t Claimed with
     | Claimed ->
       let result, ~tokens =
         Wait.Contended.perform (Parallel_kernel1.handler_exn parallel) (Promise t)
       in
       #(result, ~tokens)
     | Ready { result; tokens } -> #(result, ~tokens)
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
        let #(result, ~tokens) =
          Parallel_kernel1.with_parallel job ~scheduler ~tokens ~password ~handler
        in
        fill t (Result.Capsule.globalize result) ~scheduler ~tokens)
    in
    ()
  | Compare_failed -> ()
;;

let[@inline] create_fiber t job ~scheduler ~tokens ~key =
  let #({ many = cont }, key) =
    Capsule.Key.access key ~f:(fun [@inline] access ->
      let k =
        Wait.Contended.fiber (fun handler () -> apply t job ~scheduler ~tokens ~handler)
      in
      { many = Capsule.Data.wrap_unique ~access k })
  in
  #(cont, key)
;;

let fiber_exn t job ~scheduler ~tokens =
  let (P key) = Capsule.create () in
  let #(cont, key) = create_fiber t job ~scheduler ~tokens ~key in
  fun () -> continue () ~scheduler ~key ~cont
;;

let try_fiber t job ~scheduler ~tokens () =
  match Unique.Atomic.get (Obj.magic_unique t) with
  | Claimed -> () (* Another worker is responsible for this job. *)
  | _ ->
    let (P key) = Capsule.create () in
    (* If we fail to allocate a fiber, we drop the job without claiming the promise. *)
    (match create_fiber t job ~scheduler ~tokens ~key with
     | #(cont, key) -> continue () ~scheduler ~key ~cont
     | exception Out_of_fibers -> ())
;;
