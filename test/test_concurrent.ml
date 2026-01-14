open! Core
open! Import
open! Await
open Portable_test_helpers

let rec fib_par parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let #(a, b) =
      Parallel.fork_join2
        parallel
        (fun parallel -> fib_par parallel (n - 1))
        (fun parallel -> fib_par parallel (n - 2))
    in
    a + b
;;

module Test_scheduler (Scheduler : Parallel.Scheduler.S_concurrent) = struct
  let with_scheduler ?max_domains f =
    let scheduler = Scheduler.create ?max_domains () in
    f scheduler;
    Scheduler.stop scheduler
  ;;

  let global_scope =
    Scope.Global.create () ~on_exit:(fun _scope exn -> assert (Or_null.is_null exn))
  ;;

  let schedule_async scheduler ~f =
    let scheduler = Scheduler.Expert.scheduler scheduler in
    Concurrent.Scheduler.spawn
      scheduler
      global_scope
      (Concurrent.task (fun _scope _parallel concurrent ->
         f (Concurrent.await concurrent) [@nontail]))
  ;;

  let%expect_test "basic concurrent-parallel" =
    let x, y = Atomic.make 0, Atomic.make 0 in
    let scheduler = Parallel_scheduler.create () in
    Parallel_scheduler.concurrent
      scheduler
      ~terminator:Terminator.never
      ~f:(fun concurrent ->
        Concurrent.with_scope concurrent () ~f:(fun spawn ->
          Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
            Atomic.set x (fib_par parallel 10));
          Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
            Atomic.set y (fib_par parallel 10))));
    Parallel_scheduler.stop scheduler;
    printf "%d %d\n" (Atomic.get x) (Atomic.get y);
    [%expect {| 89 89 |}]
  ;;

  let%expect_test "schedule_async does not block" =
    (* This test relies on there being two domains, as it blocks on the async task. *)
    with_scheduler (fun scheduler ->
      let barrier = Barrier.create 2 in
      schedule_async scheduler ~f:(fun _ ->
        (* If schedule_async blocks, this barrier will never be lifted *)
        Barrier.await barrier);
      Barrier.await barrier)
  ;;

  let%expect_test "schedule_async->stop on one domain doesn't deadlock" =
    let n = Atomic.make 0 in
    with_scheduler ~max_domains:1 (fun scheduler ->
      schedule_async scheduler ~f:(fun _ -> Atomic.incr n);
      Atomic.incr n);
    (* Stopping the scheduler guarantees the async task finishes *)
    assert (Atomic.get n = 2)
  ;;

  let%expect_test "schedule_async->yield on one domain doesn't deadlock" =
    let n = Atomic.make 0 in
    with_scheduler ~max_domains:1 (fun scheduler ->
      schedule_async scheduler ~f:(fun _ -> Atomic.incr n);
      while Atomic.get n = 0 do
        Basement.Stdlib_shim.Domain.cpu_relax ()
      done);
    (* Stopping the scheduler guarantees the async task finishes *)
    assert (Atomic.get n = 1)
  ;;

  let%expect_test "locking in sibling tasks doesn't deadlock" =
    let (P key) = Capsule.Expert.create () in
    let mutex = Mutex.create key in
    with_scheduler (fun scheduler ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let #((), ()) =
          Parallel.fork_join2
            parallel
            (fun parallel ->
              Mutex.with_access (Await_blocking.await Terminator.never) mutex ~f:(fun _ ->
                let #((), ()) =
                  Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
                in
                ())
              [@nontail])
            (fun parallel ->
              Mutex.with_access (Await_blocking.await Terminator.never) mutex ~f:(fun _ ->
                let #((), ()) =
                  Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
                in
                ())
              [@nontail])
        in
        ()));
    [%expect {| .... |}]
  ;;

  let%expect_test "locking in async tasks doesn't deadlock" =
    let (P key) = Capsule.Expert.create () in
    let mutex = Mutex.create key in
    with_scheduler ~max_domains:1 (fun scheduler ->
      schedule_async scheduler ~f:(fun await ->
        Mutex.with_access await mutex ~f:(fun _ -> printf "."));
      schedule_async scheduler ~f:(fun await ->
        Mutex.with_access await mutex ~f:(fun _ -> printf "."));
      Scheduler.parallel scheduler ~f:(fun parallel ->
        Mutex.with_access (Await_blocking.await Terminator.never) mutex ~f:(fun _ ->
          let #((), ()) =
            Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
          in
          ())
        [@nontail]));
    [%expect {| .... |}]
  ;;

  let%expect_test "schedule_async respects stop" =
    Expect_test_helpers_core.require_does_raise (fun () ->
      with_scheduler (fun scheduler ->
        Scheduler.stop scheduler;
        schedule_async scheduler ~f:(fun _ -> ())));
    [%expect {| (Failure "The scheduler is already stopped") |}]
  ;;
end

module _ = Test_scheduler (Parallel_scheduler)
