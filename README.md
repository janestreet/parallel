
# Parallelism

`lib/parallel` provides a fork-join interface for parallelism in multi-core OCaml.

To expose an opportunity for parallelism, user code calls a `fork_join` function, such as:

```ocaml
(** [fork_join2 t f g] runs [f] and [g] as parallel tasks and returns their results. If
    either task panics or raises an uncaught exception, this operation will raise [Panic]
    once both tasks have completed or panicked.

    Child tasks must not block on each other or the parent task, but they may take locks. *)
val fork_join2
  :  t @ local
  -> (t @ local -> 'a) @ once portable unyielding
  -> (t @ local -> 'b) @ once portable unyielding
  -> 'a * 'b
```

The two functions passed to `fork_join2` will (potentially) be executed in parallel.
After both functions return, `fork_join2` returns a tuple containing both results.
Fork-joins may be arbitrarily nested and do not require spawning a domain for each task.

The type `Parallel.t` represents an implementation of parallelism.
To receive a `Parallel.t`, a parallel computation must be submitted to a
separate scheduler library. Schedulers provide the following function:

```ocaml
  (** [schedule t ~monitor ~f] submits [f] to the scheduler [t] and waits for it to
      complete before returning. If [f] raises an uncaught exception, the incident is
      reported to [monitor]. *)
  val schedule
    :  t
    -> monitor:Panic.Monitor.t
    -> f:(Parallel.t @ local -> 'a) @ once portable unyielding
    -> 'a
```

Calling `schedule` provides your parallel computation with a local `Parallel.t`
that represents the ability to run parallel tasks on this scheduler.

The currently available schedulers are:

- `Parallel.Sequential`, which runs all tasks on the primary domain.

- `Parallel_scheduler_stack`, which creates a pool of worker domains that
  pull tasks from a global stack.

- `Parallel_scheduler_work_stealing`, which creates a pool of worker domains that
  pull tasks from per-domain work-stealing dequeues.

## Data Race Freedom

`lib/parallel` makes use of modes to prohibit data races at compile time.
The primary restriction on user programs is that they must provide
_portable_ functions to `fork_join`. In brief, portable functions do not
close over unprotected mutable state. Refer to the data-race-freedom
documentation for further detail.

## Asynchronous Operations

Although `lib/parallel` allows spawning asynchronous tasks, it does not yet support
awaiting their results, nor any other non-blocking operations. For example, all IO
is blocking, as the scheduler does not know how to suspend a task waiting on a
non-blocking IO event.

However, tasks are allowed to lock capsule mutexes: if the mutex is contended,
there is always a domain available to run the task holding the lock.

## Full Example

```ocaml
let rec fib parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> fib parallel (n - 1))
        (fun parallel -> fib parallel (n - 2))
    in
    a + b
;;

let fib_sequential n =
  let monitor = Parallel.Monitor.create_root () in
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Parallel.Scheduler.Sequential.schedule scheduler ~monitor ~f:(fun parallel ->
    printf "%d" (fib parallel n))
;;

let fib_parallel n =
  let monitor = Parallel.Monitor.create_root () in
  let scheduler = Parallel_scheduler_stack.create () in
  Parallel_scheduler_stack.schedule scheduler ~monitor ~f:(fun parallel ->
    printf "%d" (fib parallel n));
  Parallel_scheduler_stack.stop scheduler
;;
```
