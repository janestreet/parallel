
# Parallelism

`lib/parallel` provides a fork-join interface for parallelism in multi-core OxCaml.

To expose an opportunity for parallelism, user code calls a `fork_join` function, such as:

```ocaml
(** [fork_join2 t f g] runs [f] and [g] as parallel tasks and returns their results. If
    either task raises, this operation will reraise the leftmost exception after both
    tasks have completed or raised. Child tasks must not block on each other or the parent
    task, but they may take locks.

    [f] and [g] are [shareable], so can capture both [shared] and [uncontended]
    references. This allows the tasks to read (but not mutate) state from the environment.
    [f] is also [forkable], so cannot capture capsule passwords. *)
val fork_join2
  :  t @ local
  -> (t @ local -> 'a) @ forkable local once shareable
  -> (t @ local -> 'b) @ once shareable
  -> #('a * 'b)
```

The two functions passed to `fork_join2` will (potentially) be executed in parallel.
After both functions return, `fork_join2` returns a tuple containing both results.
Fork-joins may be arbitrarily nested and do not require creating a thread or fiber
for each task.

The type `Parallel.t` represents an implementation of parallelism.
To receive a `Parallel.t`, a parallel computation must be submitted to a
separate scheduler library. Schedulers provide the following function:

```ocaml
  (** [parallel t ~f] creates an implementation of parallelism backed by [t], applies [f],
      and waits for it to complete. *)
  val parallel : t -> f:(parallel @ local -> 'a) @ once shareable -> 'a
```

Calling `schedule` provides your parallel computation with a local `Parallel.t`
that represents the ability to run parallel tasks on this scheduler.

The currently available schedulers are:

- `Parallel.Sequential`, which runs all tasks on the domain that created it.

- `Parallel_scheduler`, which creates a pool of worker domains that
  pull tasks from per-domain work-stealing dequeues.

# Concurrency

The work-stealing scheduler integrates with `lib/concurrent` to provide non-blocking
operations. More documentation coming soon.

# Data Race Freedom

`lib/parallel` makes use of modes to prohibit data races at compile time.
The primary restriction on user programs is that they must provide
_portable_ functions to `fork_join`. In brief, portable functions do not
close over unprotected mutable state. Refer to the
[data-race-freedom documentation](https://oxcaml.org/documentation/parallelism/01-intro/)
for further detail.

# Full Example

```ocaml
let rec fib parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let #(a, b) =
      Parallel.fork_join2
        parallel
        (fun parallel -> fib parallel (n - 1))
        (fun parallel -> fib parallel (n - 2))
    in
    a + b
;;

let fib_sequential n =
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->
    printf "%d" (fib parallel n))
;;

let fib_parallel n =
  let scheduler = Parallel_scheduler.create () in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    printf "%d" (fib parallel n));
  Parallel_scheduler.stop scheduler
;;
```
