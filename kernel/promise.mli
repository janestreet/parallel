@@ portable

open! Base
open! Import

include module type of struct
  include Parallel_kernel0.Promise
end

val start : unit -> 'a t

(** [fiber_exn t job ~scheduler ~tokens ~lazy_] attaches [t] to [job] and allocates a
    fiber [f] that may be executed on another domain. [f] starts with [tokens] promotion
    tokens.

    Applying [f] returns after one of three conditions are met.

    - If [t] has already been awaited, [f] returns immediately. Otherwise, [f] applies
      [job] with a fresh [Parallel.t] using [scheduler] and [tokens].

    - If applying [job] suspends the current fiber, [f] returns immediately. This occurs
      if [job] awaits another promise or an [Await.Trigger.t]. When the promise is filled
      or the trigger is signaled, [f] is resubmitted to [scheduler].

    - If [job] returns a result and another fiber is awaiting [t], [f] resubmits the
      awaiter to [scheduler]. Otherwise, [f] stores the result in [t].

    @raise Out_of_fibers
      if unable to allocate a fiber. If [lazy_] is set, the promise's executor will raise
      this execption to top level. *)
val fiber_exn
  :  'a t
  -> 'a Parallel_kernel1.Job.t @ once portable
  -> scheduler:Parallel_kernel0.Scheduler.t
  -> tokens:int
  -> lazy_:bool
  -> (unit -> unit) @ once portable

(** Like [fiber], but does not attempt to allocate the fiber until [f] is applied. If [f]
    fails to allocate the fiber, it returns without executing [job] or claiming [t]. *)
val try_fiber
  :  'a t
  -> 'a Parallel_kernel1.Job.t @ once portable
  -> scheduler:Parallel_kernel0.Scheduler.t
  -> tokens:int
  -> (unit -> unit) @ once portable

(** [await t job parallel] checks the state of the fiber [f] associated with [t].

    - If [f] has completed, [await] returns its result.

    - If [f] is pending, [await] suspends the current fiber until [f] completes.

    - If [f] has not yet started, [await] directly applies [job parallel]. When this
      occurs, applying [f] will return immediately.

    In all cases, [await] returns the result of applying [job] to some [Parallel.t].

    Because [await] may suspend the current fiber, it may only be called while running
    within a fiber created by [fiber]. Further, the promise [t] must have been attached to
    [job] by a previous call to [fiber t job]. *)
val await_or_run
  :  'a t
  -> 'a Parallel_kernel1.Job.t @ once portable
  -> Parallel_kernel1.t @ local
  -> 'a Result.Capsule.t @ local unique
