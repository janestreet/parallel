@@ portable

open! Base
open Await

(** A global mpmc queue and a collection of local work-stealing deques. *)
type t : value mod contended portable

(** Creates queues for [workers] worker threads. *)
val create : workers:int -> t

(** Inject a task from outside the scheduler. The task is pushed to the global queue.

    Makes no assumptions about the current thread and may be called concurrently. *)
val inject : t -> (unit -> unit) @ once portable -> unit

(** Push a task onto the current worker's deque.

    The current thread must be a worker managed by [Multicore] with domain id less than
    [length t]. [push] and [work] must not be called concurrently for this [t]. *)
val push : t -> (unit -> unit) @ once portable -> unit

(** Dequeues and runs tasks from the current worker's deque. If no local tasks are
    available, attempts to steal tasks from other workers' deques. If no local tasks are
    available, pops tasks from the global queue. If no global tasks are available, sleeps
    until signaled by [inject] or [try_wake].

    The current thread must be a worker managed by [Multicore] with domain id less than
    [length t]. [push] and [work] must not be called concurrently for this [t]. *)
val work : t -> await:Await.t @ local -> cancellation:Cancellation.t @ local -> unit

(** Attempts to wake [n] arbitrary workers that are currently asleep. *)
val try_wake : t -> n:int -> unit

(** Number of local queues. *)
val length : t -> int
