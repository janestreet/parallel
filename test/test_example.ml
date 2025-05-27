open! Core
open! Import

[@@@alert "-experimental"]

(* $MDX part-begin=fib *)
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

(* $MDX part-end *)

let%expect_test "fib_sequential" =
  fib_sequential 10;
  [%expect {| 89 |}]
;;

let%expect_test "fib_parallel" =
  if is_runtime5 then fib_parallel 10 else fib_sequential 10;
  [%expect {| 89 |}]
;;
