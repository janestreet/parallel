open Base
open Await
module Atomic = Portable.Atomic
module Scheduler = Parallel_kernel.For_scheduler

module Mpmc : sig @@ portable
  type t : value mod contended portable

  val create : unit -> t
  val push : t -> (unit -> unit) @ once portable -> unit
  val pop : t -> (unit -> unit) or_null @ once portable
end = struct
  type t = (unit -> unit) aliased Portable_mpmc_queue.t

  let create () = Portable_mpmc_queue.create ~padded:true ()
  let[@inline] push t f = Portable_mpmc_queue.push t { aliased = f }

  let[@inline] pop t =
    match Portable_mpmc_queue.pop t with
    | This { aliased } -> This aliased
    | Null -> Null
  ;;
end

module Wsdeq : sig @@ portable
  type t : value mod portable

  val create : unit -> t
  val push : t -> (unit -> unit) @ once portable -> unit
  val pop : t -> (unit -> unit) or_null @ once portable
  val steal : t @ contended -> (unit -> unit) or_null @ once portable
end = struct
  type t = (unit -> unit) aliased Portable_ws_deque.t

  let create = Portable_ws_deque.create

  (* [push] and [pop] are mutually non-reentrant, but the heartbeat may call [push]
     concurrently, so it must be disabled. *)

  let[@inline] push t f =
    Scheduler.without_heartbeat (fun [@inline] () ->
      Portable_ws_deque.push t { aliased = f })
    [@nontail]
  ;;

  let[@inline] pop t =
    match
      Scheduler.without_heartbeat (fun [@inline] () ->
        { portended = Portable_ws_deque.pop t })
    with
    | { portended = This { aliased } } -> This aliased
    | { portended = Null } -> Null
  ;;

  let[@inline] steal t =
    match Portable_ws_deque.steal t with
    | This { aliased } -> This aliased
    | Null -> Null
  ;;
end

type worker =
  | P :
      { queue : Wsdeq.t @@ contended
      ; sleepy : bool Await.Awaitable.t
      ; mutex : 'k Sync.Mutex.t
      }
      -> worker

type t =
  { global_queue : Mpmc.t
  ; workers : worker Iarray.t
  ; sleepers : int Atomic.t
  }

let create_worker () =
  let queue = Wsdeq.create () in
  let sleepy = Await.Awaitable.make ~padded:true false in
  (* NOTE: We're not actually protecting any data in this mutex's capsule; we're just
     using it to synchronize going to sleep and waking up. *)
  let (P key) = Capsule.Prim.create () in
  let mutex = Sync.Mutex.create key in
  P { queue; sleepy; mutex }
;;

let create ~workers =
  let global_queue = Mpmc.create () in
  let workers = Iarray.init workers ~f:(fun _ -> create_worker ()) in
  let sleepers = Atomic.make ~padded:true 0 in
  { global_queue; workers; sleepers }
;;

let length t = Iarray.length t.workers

let[@inline] wake { workers; _ } ~idx =
  let (P { mutex; sleepy; _ }) = Iarray.get workers idx in
  (* We must lock before checking [sleepy] so we don't miss workers that have run out of
     work but not yet set [sleepy]. Clearing [sleepy] races with [try_wait], which is okay
     because [try_wake] is guaranteed to wake up the worker if it wins the race.

     We use a spinlock here because we don't expect this lock to ever be contended for
     very long; all critical sections are bounded and short. *)
  Sync.Mutex.with_access Sync.spinning mutex ~f:(fun _ _ : bool ->
    (* We first [Atomic.get] because it's more efficient (on x86) to do a nonatomic load
       to check that we want to attempt waking before the atomic exchange, which locks the
       cache line (test-and-test-and-set). *)
    let wake = Await.Awaitable.get sleepy in
    if wake && Await.Awaitable.exchange sleepy false then Await.Awaitable.signal sleepy;
    wake)
;;

let[@inline] wake_one t =
  let len = Iarray.length t.workers in
  let start = Random.int len in
  let rec aux i =
    if i < len
    then (
      let idx = start + i in
      (* start < len, i < len -> idx < 2 * len *)
      let idx = Bool.select (idx >= len) (idx - len) idx in
      if not (wake t ~idx) then aux (i + 1))
  in
  aux 0
;;

let[@inline] try_wake { workers; sleepers; _ } ~n =
  (* This is not atomic with respect to stealing and updating [sleepers] in [work], so it
     may drop wakeups. Using this function to wake stealers means there could be work in
     our queue yet all other workers go to sleep. However, we will try again whenever we
     spawn an additional job, so we're serializing at most one fork per failure. This
     makes the fast path a single [sleepers > 0] check.

     Using a bitfield would let us get an index to wake by tzcnting sleepers, but it's not
     clear this would be better, since waking up a workers would require atomic-anding out
     the set bit on the shared [sleepers] instead of exchanging an exclusive [sleepy].
     Either way, the [sleepers <> 0] case should be rare in real workloads, so it probably
     doesn't matter that much. *)
  let s = Atomic.get sleepers in
  if s > 0
  then (
    let n = Int.min n s in
    let len = Iarray.length workers in
    let start = Random.int len in
    let rec find i ~n =
      if i < len && n > 0
      then (
        let j = start + i in
        (* start < len, i < len -> j < 2 * len *)
        let j = Bool.select (j >= len) (j - len) j in
        (* Safety: 0 <= j < len = Iarray.length workers *)
        let (P { mutex; sleepy; _ }) = Iarray.unsafe_get workers j in
        if Await.Awaitable.get sleepy
        then (
          if Await.Awaitable.exchange sleepy false
          then
            (* Lock to wait until the queue is actually sleeping. *)
            Sync.Mutex.with_access Sync.spinning mutex ~f:(fun _ _ ->
              Await.Awaitable.signal sleepy);
          find (i + 1) ~n:(n - 1))
        else find (i + 1) ~n)
    in
    find 0 ~n [@nontail])
;;

let steal workers ~idx =
  let n = Iarray.length workers in
  let start = Random.int n in
  let rec aux i =
    if i < n
    then (
      let j = start + i in
      (* start < len, i < len -> j < 2 * len *)
      let j = Bool.select (j >= n) (j - n) j in
      if j = idx
      then aux (i + 1)
      else (
        (* Safety: 0 <= j < len = Iarray.length workers *)
        let (P { queue; _ }) = Iarray.unsafe_get workers j in
        match Wsdeq.steal queue with
        | This _ as task -> task
        | Null -> aux (i + 1)))
    else Null
  in
  aux 0 [@nontail]
;;

let inject t f =
  Mpmc.push t.global_queue f;
  wake_one t
;;

let push { workers; _ } f =
  let idx = Multicore.current_domain () in
  (* Safety: called from domain with id less than [Iarray.length workers]. *)
  let (P { queue = self; _ }) =
    assert%debug (idx < Iarray.length workers);
    Iarray.unsafe_get workers idx
  in
  (* Safety: exactly one thread accesses [self] at [uncontended]. *)
  Wsdeq.push (Obj.magic_uncontended self) f
;;

let work { global_queue; workers; sleepers } ~await ~cancellation =
  let idx = Multicore.current_domain () in
  let (P { queue = self; sleepy; mutex }) = Iarray.get workers idx in
  let sync = Obj.magic_unyielding (Await.sync await) in
  let rec go spin cw key =
    let task =
      match
        Wsdeq.pop
          (* Safety: exactly one thread accesses [self] at [uncontended]. *)
          (Obj.magic_uncontended self)
      with
      | This task -> This task
      | Null ->
        (* We could prioritize either stealing or popping from the global queue here.
           Although stealing would help the currently running job(s) complete faster,
           steals hurt locality, and postponing steals makes them less likely to allocate
           a fiber. Hence, popping from the global queue increases overall throughput (and
           fairness to an extent). Note we must never prioritize the global queue over the
           local queue, as this would lead to deadlocks. *)
        (match Mpmc.pop global_queue with
         | This task -> This task
         | Null ->
           (match steal workers ~idx with
            | This task -> This task
            | Null -> Null))
    in
    match task with
    | This task ->
      let #(_, key) =
        Sync.Mutex.Condition.Wait.release_temporarily cw key ~f:(fun _ ->
          task ();
          Sync.yield sync)
      in
      go spin cw key
    | Null ->
      Atomic.incr sleepers;
      Await.Awaitable.set sleepy true;
      let #(outcome, key) =
        Sync.Mutex.Condition.Wait.release_temporarily cw key ~f:(fun await ->
          (* Now we must use the underlying scheduler's sync. *)
          let await = (Await.Expert.with_sync [@alloc stack]) await sync in
          Await.Awaitable.await_or_cancel
            await
            cancellation
            sleepy
            ~until_phys_unequal_to:true [@nontail])
      in
      Atomic.decr sleepers;
      (match outcome with
       | Canceled -> #((), key)
       | Signaled -> go spin cw key
       | Terminated ->
         (match (raise Await.Terminated : Nothing.t) with
          | _ -> .))
  in
  (* We use spinning for operations on the queue mutex, since we only want to suspend the
     worker once we know there's no work to do. *)
  let await = (Await.Expert.with_sync [@alloc stack]) await Sync.spinning in
  Sync.Mutex.with_key_and_condition_wait_poisoning await mutex ~f:go [@nontail]
;;
