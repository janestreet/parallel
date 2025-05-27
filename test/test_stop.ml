open! Core
open! Import

let monitor = Parallel.Monitor.create_root ()

let rec fib_par parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> fib_par parallel (n - 1))
        (fun parallel -> fib_par parallel (n - 2))
    in
    a + b
;;

module Test_scheduler (Scheduler : Common.Scheduler) = struct
  let%expect_test "exceptions" =
    let scheduler = Scheduler.configure (Scheduler.create [@alert "-experimental"]) () in
    Scheduler.stop scheduler;
    Expect_test_helpers_core.require_does_raise (fun () -> Scheduler.stop scheduler);
    [%expect {| (Failure "The scheduler is already stopped") |}];
    Expect_test_helpers_core.require_does_raise (fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun _ -> printf "Did not raise."));
    [%expect {| (Failure "The scheduler is already stopped") |}]
  ;;

  let%expect_test "stop doesn't deadlock" =
    for _ = 1 to 1000 do
      let scheduler =
        Scheduler.configure (Scheduler.create [@alert "-experimental"]) ()
      in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        ignore (fib_par parallel 2 : int));
      Scheduler.stop scheduler
    done
  ;;
end

include Common.Test_schedulers (Test_scheduler)
