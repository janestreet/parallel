open! Base
open Await

(* $MDX part-begin=parallel *)

(** [with_parallel ?max_workers f] creates a scheduler that uses up to [max_workers]
    worker threads, spawns [f] into it, and blocks the current thread until [f] is done
    executing. Returns the result of [f].

    Creating a scheduler is an expensive operation, and should ideally only happen at
    process startup. Most users should use the functions in the [Parallel_command] library
    to create their scheduler. *)
val with_parallel
  :  ?max_workers:int (* Default: [Multicore.max_domains ()] *)
  -> (Parallel_kernel.t @ local -> 'a) @ once
  -> 'a

(* $MDX part-end *)

(** [with_concurrent ?max_workers f] creates a scheduler that uses up to [max_workers]
    worker threads, passes it to [f], and blocks the current thread until [f] is done
    executing. Returns the result of [f].

    Concurrent tasks are executed in order of submission, so later tasks are not
    guaranteed to run until the current task returns or yields.

    Creating a scheduler is an expensive operation, and should ideally only happen at
    process startup. Most users should use the functions in the [Parallel_command] library
    to create their scheduler. *)
val with_concurrent
  :  ?max_workers:int (* Default: [Multicore.max_domains ()] *)
  -> (Parallel_kernel.t Concurrent.t @ local portable -> 'a) @ local once unyielding
  -> 'a

(** [scheduler ?max_workers ?on_root ()] creates a concurrent scheduler that spawns up to
    [max_workers] worker tasks.

    If [on_root] is [None] and [Multicore.max_domains () > 1], no worker is spawned on the
    root domain. If [on_root] is [Some sched], one worker will be spawned onto [sched]
    with affinity zero. [sched] is required to run this worker on the root domain, where
    it runs concurrently with the existing program.

    Creating a scheduler is an expensive operation, and should ideally only happen at
    process startup. Most users should use the functions in the [Parallel_command] library
    to create their scheduler. *)
val scheduler
  :  ?max_workers:int (* Default: [Multicore.max_domains ()] *)
  -> ?on_root:_ Concurrent.Scheduler.t (** Default: do not use the root domain. **)
     @ local
  -> unit
  -> Parallel_kernel.t Concurrent.Scheduler.t @ portable

(** [parallel scheduler f] spawns [f] into [scheduler] and blocks the current thread until
    [f] is done executing. Returns the result of [f].

    Concurrent tasks are executed in order of submission, so later tasks are not
    guaranteed to run until the current task returns or yields. *)
val parallel
  :  Parallel_kernel.t Concurrent.Scheduler.t @ portable
  -> (Parallel_kernel.t @ local -> Parallel_kernel.t Concurrent.t @ local portable -> 'a)
     @ once
  -> 'a
  @@ portable

(** [concurrent scheduler await f] spawns [f] into [scheduler] and uses [await] to wait
    for the result.

    Concurrent tasks are executed in order of submission, so later tasks are not
    guaranteed to run until the current task returns or yields. *)
val concurrent
  :  Parallel_kernel.t Concurrent.Scheduler.t @ portable
  -> Await.t @ local
  -> (Parallel_kernel.t Concurrent.t @ local portable -> 'a) @ local once
  -> 'a
  @@ portable
