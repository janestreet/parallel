@@ portable

open! Base

(** A collection of work-stealing deques. *)
type t : value mod contended portable

(** Creates a set of [domains] deques. *)
val create : domains:int -> t

(** Push a task onto the current domain's deque.

    The current domain must be either the initial domain or a domain managed by
    [Multicore] with id less than [length t]. At most one thread per domain may call
    [push] and [work] on this set of queues. *)
val push : t -> (unit -> unit) @ once portable -> unit

(** Dequeues and runs tasks from the current domain's deque. If no local tasks are
    available, attempts to steal tasks from other domains' deques. If no remote tasks are
    available, sleeps until signaled by [wake].

    The current domain must be either the initial domain or a domain managed by
    [Multicore] with id less than [length t]. At most one thread per domain may call
    [push] and [work] on this set of queues. *)
val work : t -> break:(unit -> bool) @ local portable -> unit

(** Wakes the worker at index [idx], if it is currently asleep. *)
val wake : t -> idx:int -> unit

(** Wakes one worker, if any are currently asleep. *)
val wake_one : t -> unit

(** Attempts to wake [n] arbitrary workers that are currently asleep. *)
val try_wake : t -> n:int -> unit

(** Number of queues. *)
val length : t -> int
