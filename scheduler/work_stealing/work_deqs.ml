open! Base
module Atomic = Portable.Atomic
module Capsule = Portable.Capsule.Expert

module Capsule_with_mutex = struct
  type ('a, 'k) inner =
    { mutex : 'k Capsule.Mutex.t
    ; data : ('a, 'k) Capsule.Data.t
    }

  type 'a t = P : ('a, 'k) inner -> 'a t [@@unboxed]
end

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
  { mutex : 'k Capsule.Mutex.t
  ; cond : 'k Capsule.Condition.t
  ; stealer : Once_deq.t @@ contended
  ; sleepy : bool Atomic.t
  }

type queue : value mod contended portable = P : 'k queue_inner -> queue [@@unboxed]

type t =
  { queues : queue Iarray.t
  ; sleepers : int Atomic.t
  }

let create_one () =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let (P self_key) = Capsule.create () in
  let self_mutex = Capsule.Mutex.create self_key in
  let cond = Capsule.Condition.create () in
  let queue = Capsule.Data.create Once_deq.create in
  let sleepy = Atomic.make_alone false in
  ( Capsule_with_mutex.P { mutex = self_mutex; data = queue }
  , P { mutex; cond; stealer = Capsule.Data.project queue; sleepy } )
;;

let create ~domains =
  let owners, queues = Iarray.init domains ~f:(fun _ -> create_one ()) |> Iarray.unzip in
  let sleepers = Atomic.make_alone 0 in
  owners, { queues; sleepers }
;;

let[@inline] wake { queues; _ } ~idx =
  let (P { mutex; cond; _ }) = Iarray.get queues idx in
  (* Must be atomic with respect to [steal_or_break] and going to sleep in [work]. *)
  Capsule.Mutex.with_lock mutex ~f:(fun _ -> Capsule.Condition.signal cond) [@nontail]
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
        let j = (start + i) % len in
        (* Safety: 0 <= j < len = Iarray.length queues *)
        let (P { mutex; cond; sleepy; _ }) = Iarray.unsafe_get queues j in
        (* Clear sleepy so others don't try to wake this queue.

           We first [Atomic.get &&] because it's more efficient (on x86) to do a nonatomic
           load to check that we want to attempt waking before the atomic exchange, which
           locks the cache line (cf test-and-test-and-set). *)
        if Atomic.get sleepy && Atomic.exchange sleepy false
        then (
          (* Lock to wait until the queue is actually sleeping. *)
          Capsule.Mutex.with_lock mutex ~f:(fun _ -> Capsule.Condition.signal cond);
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
      let j = (start + i) % n in
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
  let (P { mutex; cond; sleepy; _ }) = Iarray.get queues idx in
  let rec steal_or_break key =
    match steal queues ~idx with
    | This _ as task -> task, key
    | Null when break () -> Null, key
    | Null ->
      Atomic.incr sleepers;
      Atomic.set sleepy true;
      let key = Capsule.Condition.wait cond ~mutex key in
      Atomic.set sleepy false;
      Atomic.decr sleepers;
      steal_or_break key
  in
  let rec go () =
    match Once_deq.pop self with
    | This task ->
      task ();
      go ()
    | Null ->
      (match Capsule.Mutex.with_key mutex ~f:steal_or_break with
       | This task ->
         task ();
         go ()
       | Null -> ())
  in
  go () [@nontail]
;;
