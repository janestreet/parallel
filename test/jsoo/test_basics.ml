open! Core
module Option_u = Unboxed_datatypes.Option_u

let parallel f = f Parallel.sequential

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

let%expect_test "fib_par" =
  parallel (fun parallel -> printf "%d" (fib_par parallel 10));
  [%expect {| 89 |}]
;;

let%expect_test "f3" =
  parallel (fun parallel ->
    let #(a, b, c) =
      Parallel.fork_join3 parallel (fun _ -> 1) (fun _ -> 2) (fun _ -> 3)
    in
    printf "%d" (a + b + c));
  [%expect {| 6 |}]
;;

let%expect_test "f4" =
  parallel (fun parallel ->
    let #(a, b, c, d) =
      Parallel.fork_join4 parallel (fun _ -> 1) (fun _ -> 2) (fun _ -> 3) (fun _ -> 4)
    in
    printf "%d" (a + b + c + d));
  [%expect {| 10 |}]
;;

let%expect_test "f5" =
  parallel (fun parallel ->
    let #(a, b, c, d, e) =
      Parallel.fork_join5
        parallel
        (fun _ -> 1)
        (fun _ -> 2)
        (fun _ -> 3)
        (fun _ -> 4)
        (fun _ -> 5)
    in
    printf "%d" (a + b + c + d + e));
  [%expect {| 15 |}]
;;

let%expect_test "for" =
  parallel (fun parallel ->
    let a = Atomic.make 0 in
    Parallel.for_ parallel ~start:0 ~stop:10 ~f:(fun _ i -> Atomic.add a i);
    printf "%d" (Atomic.get a));
  [%expect {| 45 |}]
;;

let%expect_test "fold" =
  parallel (fun parallel ->
    let fold_n n =
      (Parallel.fold [@kind value_or_null (value_or_null & value_or_null)])
        parallel
        ~init:(fun () -> 0)
        ~state:(#(~start:0, ~stop:n) : #(start:int * stop:int))
        ~next:(fun _ acc #(~start, ~stop) ->
          if start = stop
          then
            (Option_u.none
            [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
              ()
          else
            (Option_u.some
            [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
              #(acc + 1, #(~start:(start + 1), ~stop)))
        ~stop:(fun _ i -> i)
        ~fork:(fun _ #(~start, ~stop) ->
          let pivot = start + ((stop - start) / 2) in
          if pivot <= start + 1
          then
            (Option_u.none
            [@kind
              (_
               : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null)))])
              ()
          else
            (Option_u.some
            [@kind
              (_
               : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null)))])
              #(#(~start, ~stop:pivot), #(~start:pivot, ~stop)))
        ~join:(fun _ a b -> a + b)
    in
    printf "%d\n" (fold_n 10);
    printf "%d\n" (fold_n 10_000));
  [%expect
    {|
    10
    10000
    |}]
;;
