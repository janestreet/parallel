open! Base
open! Import

let%expect_test "result doesn't allocate" =
  (match
     Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
       Parallel.For_scheduler.Result.try_with (fun () -> 1))
   with
   | Ok n -> assert (n = 1)
   | _ -> assert false);
  [%expect {| |}]
;;

let%expect_test "capsule doesn't allocate" =
  (match
     Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
       Parallel.For_scheduler.Result.Capsule.try_with (fun () -> 1))
   with
   | Ok (n, _) -> assert (Capsule.Data.project n = 1)
   | _ -> assert false);
  [%expect {| |}]
;;

let%expect_test ("sequential scheduler doesn't allocate" [@tags "fast-flambda"]) =
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Expect_test_helpers_core.require_no_allocation_local (fun () ->
    Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->
      let #(x, y) = Parallel.fork_join2 parallel (fun _ -> 1) (fun _ -> 1) in
      assert (x + y = 2)));
  [%expect {| |}]
;;

let%expect_test ("fork_join doesn't allocate" [@tags "fast-flambda"]) =
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Expect_test_helpers_core.require_no_allocation_local ~cr:CR_someday (fun () ->
    Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->
      let [ x; y; z ] =
        Parallel.fork_join parallel [ (fun _ -> 1); (fun _ -> 1); (fun _ -> 1) ]
      in
      assert (x + y + z = 3)));
  [%expect
    {|
    ("allocation exceeded limit" (allocation_limit (Minor_words 0)))
    |}]
;;
