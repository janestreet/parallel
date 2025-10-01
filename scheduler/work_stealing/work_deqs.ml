open Base
module Atomic = Portable.Atomic
open Await

module Once_deq : sig @@ portable
  type t : value mod portable

  val push : t -> (unit -> unit) @ once portable -> unit
  val pop : t -> (unit -> unit) or_null @ once portable
  val steal : t @ contended -> (unit -> unit) or_null @ once portable
  val create : unit -> t
end = struct
  type t = (unit -> unit) Portable_ws_deque.t

  (* Safety: elements are pushed into the deque exactly once, and either popped or
     stolen exactly once. *)
  let[@inline] push t f =
    Portable_ws_deque.push t ((Obj.magic_many [@mode uncontended portable aliased]) f)
  ;;

  let pop = Portable_ws_deque.pop
  let steal = Portable_ws_deque.steal
  let create = Portable_ws_deque.create
end

module Self = Once_deq

type 'k queue_inner =
  { stealer : Once_deq.t @@ contended
  ; sleepy : bool Awaitable.t
  ; mutex : 'k Mutex.t
  }

type queue : value mod contended portable = P : 'k queue_inner -> queue [@@unboxed]

type t =
  { queues : queue Iarray.t
  ; sleepers : int Atomic.t
  }

let create_one () =
  let queue = Capsule.Isolated.create Once_deq.create in
  let queue, { aliased = stealer } = Capsule.Isolated.get_id_contended queue in
  let sleepy = Awaitable.make_alone false in
  (* NOTE: We're not actually protecting any data in this mutex's capsule; we're just
     using it to synchronize [stealer] and [sleepy], which are both atomic. *)
  let (P key) = Capsule.Expert.create () in
  let mutex = Mutex.create key in
  Unique.Once.Atomic.make { many = queue }, P { stealer; sleepy; mutex }
;;

let create ~domains =
  let owners, queues = Iarray.init domains ~f:(fun _ -> create_one ()) |> Iarray.unzip in
  let sleepers = Atomic.make_alone 0 in
  owners, { queues; sleepers }
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
      (* We first [Atomic.get] because it's more efficient (on x86) to do a nonatomic
         load to check that we want to attempt waking before the atomic exchange, which
         locks the cache line (test-and-test-and-set). *)
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
  (* This is not atomic with respect to stealing and updating [sleepers] in [work],
     so it may drop wakeups. Using this function to wake stealers means there could
     be work in our queue yet all other domains go to sleep. However, we will try
     again whenever we spawn an additional job, so we're serializing at most one
     fork per failure. This makes the fast path a single [sleepers > 0] check.

     Using a bitfield would let us get an index to wake by tzcnting sleepers, but it's not
     clear this would be better, since waking up a domain would require atomic-anding out
     the set bit on the shared [sleepers] instead of exchanging a non-shared [sleepy]. The
     [sleepers <> 0] case should already be vanishingly rare in real workloads, so it
     probably doesn't matter either way. *)
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
      let j = Bool.select (j >= n) (j - n) j in
      if j = idx
      then aux (i + 1)
      else (
        let (P { stealer; _ }) = Iarray.unsafe_get queues j in
        match Once_deq.steal stealer with
        | This _ as task -> task
        | Null -> aux (i + 1)))
    else Null
  in
  aux 0 [@nontail]
;;

let work { queues; sleepers } ~self ~idx ~break =
  (* We use spinning for operations on the queue mutex, since we only want to get
     descheduled if we know there's no work to do. *)
  Await_spinning.with_await Terminator.never ~f:(fun spin ->
    let (P { sleepy; mutex; _ }) = Iarray.get queues idx in
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
                Await_blocking.with_await Terminator.never ~f:(fun block ->
                  Awaitable.await block sleepy ~until_phys_unequal_to:true))
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
      match Once_deq.pop self with
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
