@@ portable

open! Base
module Capsule := Portable.Capsule

module Self : sig
  (** The uncontended local end of one deque. Nonportable so that it cannot move between
      capsules. *)
  type t : value mod portable

  val push : t -> (unit -> unit) @ once portable -> unit
end

(** A collection of work-stealing deques. *)
type t : value mod contended portable

(** Creates [domains] deques, returning an iarray of their local ends. The [Self.t] at
    index [idx] must be passed to [work ~idx], and will be woken up by calls to
    [wake ~idx]. *)
val create
  :  domains:int
  -> Self.t Capsule.Isolated.t many Unique.Once.Atomic.t Iarray.t * t

(** Dequeues and runs task from the [self] end of one deque. If none are available,
    attempts to steal tasks from the contended non-local end of other deques. If none are
    available, sleeps until signaled by [wake]. *)
val work : t -> self:Self.t -> idx:int -> break:(unit -> bool) @ local portable -> unit

(** Wakes the worker at index [idx], if it is currently asleep. *)
val wake : t -> idx:int -> unit

(** Wakes one worker, if any are currently asleep. *)
val wake_one : t -> unit

(** Attempts to wake [n] arbitrary workers that are currently asleep. *)
val try_wake : t -> n:int -> unit

(** Number of queues. *)
val length : t -> int
