open! Base
open! Import

let%expect_test "monitor doesn't allocate" =
  let monitor = Parallel.Monitor.create_root () in
  (match
     Expect_test_helpers_core.require_no_allocation_local (fun () ->
       exclave_
       Parallel.Panic.Result.handle_panics_and_report_exceptions monitor (fun () -> 1))
   with
   | Ok (n, _) -> assert (Capsule.Data.project n = 1)
   | _ -> assert false);
  [%expect {| |}]
;;

let%expect_test ("sequential scheduler doesn't allocate" [@tags "fast-flambda"]) =
  let monitor = Parallel.Monitor.create_root () in
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Expect_test_helpers_core.require_no_allocation_local (fun () ->
    Parallel.Scheduler.Sequential.schedule scheduler ~monitor ~f:(fun parallel ->
      let x, y = Parallel.fork_join2 parallel (fun _ -> 1) (fun _ -> 1) in
      assert (x + y = 2)));
  [%expect {| |}]
;;
