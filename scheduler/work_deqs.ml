open Base
open Await
module Atomic = Portable.Atomic
module Scheduler = Parallel_kernel.For_scheduler

module Once_deq : sig @@ portable
  type t : value mod portable

  val push : t -> (unit -> unit) @ once portable -> unit
  val pop : t -> (unit -> unit) or_null @ once portable
  val steal : t @ contended -> (unit -> unit) or_null @ once portable
  val create : unit -> t
end = struct
  type t = (unit -> unit) Portable_ws_deque.t

  (* [push] and [pop] are mutually non-reentrant, but the heartbeat may call [push]
     concurrently, so it must be disabled. *)

  let[@inline] push t f =
    Scheduler.without_heartbeat (fun () ->
      (* Safety: elements are pushed into the deque exactly once, and either popped or
         stolen exactly once. *)
      Portable_ws_deque.push t ((Obj.magic_many [@mode portable]) f))
    [@nontail]
  ;;

  let[@inline] pop t =
    (Scheduler.without_heartbeat (fun () -> { portended = Portable_ws_deque.pop t }))
      .portended
  ;;

  let steal = Portable_ws_deque.steal
  let create = Portable_ws_deque.create
end

type queue =
  | P :
      { queue : Once_deq.t @@ contended
      ; sleepy : bool Awaitable.t
      ; mutex : 'k Mutex.t
      }
      -> queue

type t =
  { queues : queue Iarray.t
  ; sleepers : int Atomic.t
  }

let create_one () =
  let queue = Once_deq.create () in
  let sleepy = Awaitable.make ~padded:true false in
  (* NOTE: We're not actually protecting any data in this mutex's capsule; we're just
     using it to synchronize [stealer] and [sleepy], which are both atomic. *)
  let (P key) = Capsule.Expert.create () in
  let mutex = Mutex.create key in
  P { queue; sleepy; mutex }
;;

let create ~domains =
  let queues = Iarray.init domains ~f:(fun _ -> create_one ()) in
  let sleepers = Atomic.make ~padded:true 0 in
  { queues; sleepers }
;;

let length t = Iarray.length t.queues

let[@inline] wake' { queues; _ } ~idx =
  let (P { mutex; sleepy; _ }) = Iarray.get queues idx in
  (* We must lock before checking [sleepy] so we don't miss workers that have run out of
     work but not yet set [sleepy]. Clearing [sleepy] races with [try_wait], which is okay
     because [try_wake] is guaranteed to wake up the worker if it wins the race.

     We use a spinlock here because we don't expect this lock to ever be contended for
     very long; all critical sections are bounded and short. *)
  Await_spinning.with_await Terminator.never ~f:(fun await ->
    Mutex.with_password await mutex ~f:(fun _ : bool ->
      (* We first [Atomic.get] because it's more efficient (on x86) to do a nonatomic load
         to check that we want to attempt waking before the atomic exchange, which locks
         the cache line (test-and-test-and-set). *)
      let wake = Awaitable.get sleepy in
      if wake && Awaitable.exchange sleepy false then Awaitable.signal sleepy;
      wake))
;;

let[@inline] wake t ~idx = ignore (wake' t ~idx : bool)

let[@inline] wake_one t =
  let len = Iarray.length t.queues in
  let start = Random.int len in
  let rec wake i =
    if i < len
    then (
      let idx = start + i in
      (* start < len, i < len -> idx < 2 * len *)
      let idx = Bool.select (idx >= len) (idx - len) idx in
      if not (wake' t ~idx) then wake (i + 1))
  in
  wake 0
;;

let[@inline] try_wake { queues; sleepers } ~n =
  (* This is not atomic with respect to stealing and updating [sleepers] in [work], so it
     may drop wakeups. Using this function to wake stealers means there could be work in
     our queue yet all other domains go to sleep. However, we will try again whenever we
     spawn an additional job, so we're serializing at most one fork per failure. This
     makes the fast path a single [sleepers > 0] check.

     Using a bitfield would let us get an index to wake by tzcnting sleepers, but it's not
     clear this would be better, since waking up a domain would require atomic-anding out
     the set bit on the shared [sleepers] instead of exchanging an exclusive [sleepy].
     Either way, the [sleepers <> 0] case should be rare in real workloads, so it probably
     doesn't matter that much. *)
  let s = Atomic.get sleepers in
  if s > 0
  then (
    let n = Int.min n s in
    let len = Iarray.length queues in
    let start = Random.int len in
    let rec find i ~n =
      if i < len && n > 0
      then (
        let j = start + i in
        (* start < len, i < len -> j < 2 * len *)
        let j = Bool.select (j >= len) (j - len) j in
        (* Safety: 0 <= j < len = Iarray.length queues *)
        let (P { mutex; sleepy; _ }) = Iarray.unsafe_get queues j in
        if Awaitable.get sleepy
        then (
          if Awaitable.exchange sleepy false
          then
            (* Lock to wait until the queue is actually sleeping. *)
            Await_spinning.with_await Terminator.never ~f:(fun await ->
              Mutex.with_password await mutex ~f:(fun _ -> Awaitable.signal sleepy));
          find (i + 1) ~n:(n - 1))
        else find (i + 1) ~n)
    in
    find 0 ~n [@nontail])
;;

let steal queues ~idx =
  let n = Iarray.length queues in
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
        (* Safety: 0 <= j < len = Iarray.length queues *)
        let (P { queue; _ }) = Iarray.unsafe_get queues j in
        match Once_deq.steal queue with
        | This _ as task -> task
        | Null -> aux (i + 1)))
    else Null
  in
  aux 0 [@nontail]
;;

let push { queues; _ } f =
  let idx = Multicore.current_domain () in
  (* Safety: called from domain with id less than [Iarray.length queues]. *)
  let (P { queue = self; _ }) = Iarray.unsafe_get queues idx in
  (* Safety: exactly one thread accesses [self] at [uncontended]. *)
  Once_deq.push (Obj.magic_uncontended self) f
;;

let work { queues; sleepers } ~break =
  let idx = Multicore.current_domain () in
  let (P { queue = self; sleepy; mutex }) = Iarray.get queues idx in
  (* We use spinning for operations on the queue mutex, since we only want to get
     descheduled if we know there's no work to do. *)
  Await_spinning.with_await Terminator.never ~f:(fun spin ->
    let[@inline] rec steal_or_break key =
      match steal queues ~idx with
      | This _ as task -> #(task, key)
      | Null when break () -> #(Null, key)
      | Null ->
        Atomic.incr sleepers;
        Awaitable.set sleepy true;
        let rec sleep key =
          if Awaitable.get sleepy
          then (
            match
              Mutex.release_temporarily spin mutex key ~f:(fun () ->
                (* Now we can be descheduled. *)
                Awaitable.await
                  (Await_blocking.await Terminator.never)
                  sleepy
                  ~until_phys_unequal_to:true)
            with
            | #(Signaled, key) -> sleep key
            | #(Terminated, _) ->
              Atomic.decr sleepers;
              (match raise Await.Terminated with
               | (_ : Nothing.t) -> .))
          else key
        in
        let key = sleep key in
        Atomic.decr sleepers;
        steal_or_break key
    in
    let rec go () =
      (* Safety: exactly one thread accesses [self] at [uncontended]. *)
      match Once_deq.pop (Obj.magic_uncontended self) with
      | This task ->
        task ();
        go ()
      | Null ->
        (match Mutex.with_key spin mutex ~f:steal_or_break with
         | This task ->
           task ();
           go ()
         | Null -> ())
    in
    go () [@nontail])
  [@nontail]
;;
