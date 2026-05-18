open! Core
open! Import

(* $MDX part-begin=fib *)
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

let fib_sequential n = printf "%d" (fib Parallel.sequential n)

let fib_parallel n =
  Parallel_scheduler.with_parallel (fun parallel -> printf "%d" (fib parallel n))
;;

(* $MDX part-end *)

let%expect_test "fib_sequential" =
  fib_sequential 10;
  [%expect {| 89 |}]
;;

let%expect_test "fib_parallel" =
  fib_parallel 10;
  [%expect {| 89 |}]
;;
