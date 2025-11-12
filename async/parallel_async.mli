open! Core
open! Async_kernel

(** Schedule work from Async to be run in parallel, and await the results of that work
    from Async.

    The normal way to use parallel from async is to use one of the async functions from
    {!Parallel_command} to create your application's main function. That main function
    will be passed a {{!t} [Parallel_async.t]}, allowing you to schedule functions to be
    run in parallel with {!parallel}. *)

(** Type representing an (async-capable) parallel scheduler to which tasks can be
    dispatched from [Async] *)
type t

(** [create ?monitor (module Scheduler) scheduler] creates a new async-capable parallel
    scheduler wrapping the Parallel scheduler [scheduler].

    If tasks run on this scheduler using [schedule] raise uncaught exceptions, a
    {!Parallel.Panic.Incident.t} will be reported to the async monitor [monitor], which
    defaults to the current monitor. *)
val create
  : 'sched.
  ?monitor:Async_kernel.Monitor.t
  -> (module Parallel_kernel.Scheduler.S_concurrent with type t = 'sched)
  -> 'sched
  -> t

(** [parallel t ~f] schedules [f parallel] to run on the scheduler [t] and returns a
    [Deferred.t] that will become determined with the result of [f]. [parallel] is a new
    implementation of parallelism that allows [f] to perform nested parallelism *)
val parallel
  : ('a : value mod contended).
  t -> f:(Parallel_kernel.t @ local -> 'a @ portable) @ once portable -> 'a Deferred.t

(** [concurrent t ~f] schedules [f concurrent] to run on the scheduler [t] and returns a
    [Deferred.t] that will become determined with the result of [f]. [concurrent] is a new
    implementation of concurrency that allows [f] to perform concurrency and parallelism.

    If you do not need to run concurrent tasks from [f], prefer to use [parallel].
    Notably, the parallelism scheduler is not {i fair} - if you run concurrent tasks using
    this function, they can easily starve each other out. *)
val concurrent
  : ('a : value mod contended).
  t
  -> f:(Parallel_kernel.t Concurrent.t @ local -> 'a @ portable) @ once portable
  -> 'a Deferred.t
