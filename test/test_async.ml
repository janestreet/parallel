open! Core
open! Import
open Portable_test_helpers

let on_panic panic = print_s [%message (panic : Parallel.Panic.t)]

module Test_scheduler (Scheduler : Parallel.Scheduler.S_async) = struct
  let with_scheduler ~domains ~f =
    let monitor = Parallel.Monitor.create_root () in
    let scheduler = (Scheduler.create [@alert "-experimental"]) ~domains () in
    f scheduler monitor;
    Scheduler.stop scheduler
  ;;

  let%expect_test ("schedule_async does not block" [@tags "runtime5-only"]) =
    (* This test relies on there being two domains, as it blocks on the async task. *)
    with_scheduler ~domains:2 ~f:(fun scheduler monitor ->
      let barrier = Barrier.create 2 in
      Scheduler.Expert.schedule_async scheduler ~monitor ~on_panic ~f:(fun _ ->
        (* If schedule_async blocks, this barrier will never be lifted *)
        Barrier.await barrier);
      Barrier.await barrier)
  ;;

  let%expect_test ("schedule_async on one domain doesn't deadlock"
    [@tags "runtime5-only"])
    =
    let n = Atomic.make 0 in
    with_scheduler ~domains:1 ~f:(fun scheduler monitor ->
      Scheduler.Expert.schedule_async scheduler ~monitor ~on_panic ~f:(fun _ ->
        Atomic.incr n);
      Atomic.incr n);
    (* Stopping the scheduler guarantees the async task finishes *)
    assert (Atomic.get n = 2)
  ;;

  let%expect_test ("locking in sibling tasks doesn't deadlock" [@tags "runtime5-only"]) =
    let (P key) = Capsule.create () in
    let mutex = Capsule.Mutex.create key in
    with_scheduler ~domains:2 ~f:(fun scheduler monitor ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let (), () =
          Parallel.fork_join2
            parallel
            (fun parallel ->
              Capsule.Mutex.with_lock mutex ~f:(fun _ ->
                let (), () =
                  Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
                in
                ())
              [@nontail])
            (fun parallel ->
              Capsule.Mutex.with_lock mutex ~f:(fun _ ->
                let (), () =
                  Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
                in
                ())
              [@nontail])
        in
        ()));
    [%expect {| .... |}]
  ;;

  let%expect_test ("locking in async tasks doesn't deadlock" [@tags "runtime5-only"]) =
    let (P key) = Capsule.create () in
    let mutex = Capsule.Mutex.create key in
    with_scheduler ~domains:1 ~f:(fun scheduler monitor ->
      Scheduler.Expert.schedule_async scheduler ~monitor ~on_panic ~f:(fun _ ->
        Capsule.Mutex.with_lock mutex ~f:(fun _ -> printf "."));
      Scheduler.Expert.schedule_async scheduler ~monitor ~on_panic ~f:(fun _ ->
        Capsule.Mutex.with_lock mutex ~f:(fun _ -> printf "."));
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        Capsule.Mutex.with_lock mutex ~f:(fun _ ->
          let (), () =
            Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
          in
          ())
        [@nontail]));
    [%expect {| .... |}]
  ;;

  let%expect_test ("schedule_async respects stop" [@tags "runtime5-only"]) =
    Expect_test_helpers_core.require_does_raise (fun () ->
      with_scheduler ~domains:2 ~f:(fun scheduler monitor ->
        Scheduler.stop scheduler;
        Scheduler.Expert.schedule_async scheduler ~monitor ~on_panic ~f:(fun _ -> ())));
    [%expect {| (Failure "The scheduler is already stopped") |}]
  ;;
end

module _ = Test_scheduler (Parallel_scheduler_stack)
module _ = Test_scheduler (Parallel_scheduler_work_stealing)
