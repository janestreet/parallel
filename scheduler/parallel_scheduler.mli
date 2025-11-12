open! Base

(** [t] represents a scheduler that runs each task on one member of an internal
    work-stealing pool of worker domains. *)
type t

include Parallel_kernel.Scheduler.S_concurrent with type t := t (** @inline *)
