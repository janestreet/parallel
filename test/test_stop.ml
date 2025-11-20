open! Core
open! Import

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

module Test_scheduler (Scheduler : Parallel.Scheduler.S) = struct
  let%expect_test "exceptions" =
    let scheduler = Scheduler.create () in
    Scheduler.stop scheduler;
    Expect_test_helpers_core.require_does_raise (fun () -> Scheduler.stop scheduler);
    [%expect {| (Failure "The scheduler is already stopped") |}];
    Expect_test_helpers_core.require_does_raise (fun () ->
      Scheduler.parallel scheduler ~f:(fun _ -> printf "Did not raise."));
    [%expect {| (Failure "The scheduler is already stopped") |}]
  ;;

  let%expect_test "stop doesn't deadlock" =
    for _ = 1 to 100 do
      let scheduler = Scheduler.create () in
      Scheduler.parallel scheduler ~f:(fun parallel -> ignore (fib_par parallel 2 : int));
      Scheduler.stop scheduler
    done
  ;;
end

include Common.Test_schedulers (Test_scheduler)
