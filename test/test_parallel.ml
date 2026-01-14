open! Core
open! Import

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let fib4 parallel =
  let #(x, y) =
    Parallel.fork_join2
      parallel
      (fun parallel ->
        let #(x, y) = Parallel.fork_join2 parallel (fun _ -> fib 10) (fun _ -> fib 10) in
        x + y)
      (fun parallel ->
        let #(x, y) = Parallel.fork_join2 parallel (fun _ -> fib 10) (fun _ -> fib 10) in
        x + y)
  in
  x + y
;;

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
  let scheduler = Scheduler.create ()

  let%expect_test "fib4" =
    Scheduler.parallel scheduler ~f:(fun parallel -> printf "%d" (fib4 parallel));
    [%expect {| 356 |}]
  ;;

  let%expect_test "fib_par" =
    Scheduler.parallel scheduler ~f:(fun parallel -> printf "%d" (fib_par parallel 10));
    [%expect {| 89 |}]
  ;;

  let%expect_test "f3" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let #(a, b, c) =
        Parallel.fork_join3 parallel (fun _ -> 1) (fun _ -> 2) (fun _ -> 3)
      in
      printf "%d" (a + b + c));
    [%expect {| 6 |}]
  ;;

  let%expect_test "f4" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let #(a, b, c, d) =
        Parallel.fork_join4 parallel (fun _ -> 1) (fun _ -> 2) (fun _ -> 3) (fun _ -> 4)
      in
      printf "%d" (a + b + c + d));
    [%expect {| 10 |}]
  ;;

  let%expect_test "f5" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
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

  let%expect_test "fN" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let [ a; b; c; d; e; f ] =
        Parallel.fork_join
          parallel
          [ (fun _ -> 1)
          ; (fun _ -> 2.0)
          ; (fun _ -> 3l)
          ; (fun _ -> 4L)
          ; (fun _ -> 5n)
          ; (fun _ -> 6.0s)
          ]
      in
      printf
        "%f"
        (Int.to_float a
         +. b
         +. Int32.to_float c
         +. Int64.to_float d
         +. Nativeint.to_float e
         +. Float32.to_float f));
    [%expect {| 21.000000 |}]
  ;;

  let%expect_test "for" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let a = Atomic.make 0 in
      Parallel.for_ parallel ~start:0 ~stop:10 ~f:(fun _ i -> Atomic.add a i);
      printf "%d" (Atomic.get a));
    [%expect {| 45 |}]
  ;;

  let%expect_test "fold" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
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

  let%expect_test "fold with treevec state" =
    let open struct
      type t =
        | Leaf of int Vec.t
        | Node of t * t

      let rec collect t =
        match t with
        | Leaf vec -> Vec.to_list vec
        | Node (a, b) -> collect a @ collect b
      ;;
    end in
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let fold_n n =
        (Parallel.fold [@kind value_or_null (value_or_null & value_or_null)])
          parallel
          ~init:(fun () : int Vec.t -> Vec.create ())
          ~state:(#(~start:0, ~stop:n) : #(start:int * stop:int))
          ~next:(fun _ acc #(~start, ~stop) ->
            if start = stop
            then
              (Option_u.none
              [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
                ()
            else (
              Vec.push_back acc start;
              (Option_u.some
              [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
                #(acc, #(~start:(start + 1), ~stop))))
          ~stop:(fun _ vec -> Leaf vec)
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
          ~join:(fun _ a b -> Node (a, b))
      in
      print_s [%message (collect (fold_n 10) : int list)];
      assert (List.equal Int.equal (collect (fold_n 10_000)) (List.init 10_000 ~f:Fn.id)));
    [%expect {| ("collect (fold_n 10)" (0 1 2 3 4 5 6 7 8 9)) |}]
  ;;
end

include Common.Test_schedulers (Test_scheduler)

let%expect_test "sequential ordering" =
  let scheduler = Parallel.Scheduler.Sequential.create () in
  Parallel.Scheduler.Sequential.parallel scheduler ~f:(fun parallel ->
    let _ : _ =
      Parallel.fork_join2
        parallel
        (fun _ -> print_endline "1")
        (fun _ -> print_endline "2")
    in
    print_endline "---";
    let _ : _ =
      Parallel.fork_join3
        parallel
        (fun _ -> print_endline "1")
        (fun _ -> print_endline "2")
        (fun _ -> print_endline "3")
    in
    print_endline "---";
    let _ : _ =
      Parallel.fork_join4
        parallel
        (fun _ -> print_endline "1")
        (fun _ -> print_endline "2")
        (fun _ -> print_endline "3")
        (fun _ -> print_endline "4")
    in
    print_endline "---";
    let _ : _ =
      Parallel.fork_join5
        parallel
        (fun _ -> print_endline "1")
        (fun _ -> print_endline "2")
        (fun _ -> print_endline "3")
        (fun _ -> print_endline "4")
        (fun _ -> print_endline "5")
    in
    print_endline "---";
    let _ : _ =
      Parallel.fork_join
        parallel
        [ (fun _ -> print_endline "1")
        ; (fun _ -> print_endline "2")
        ; (fun _ -> print_endline "3")
        ; (fun _ -> print_endline "4")
        ; (fun _ -> print_endline "5")
        ; (fun _ -> print_endline "6")
        ; (fun _ -> print_endline "7")
        ; (fun _ -> print_endline "8")
        ; (fun _ -> print_endline "9")
        ]
    in
    ());
  [%expect
    {|
    1
    2
    ---
    1
    2
    3
    ---
    1
    2
    3
    4
    ---
    1
    2
    3
    4
    5
    ---
    1
    2
    3
    4
    5
    6
    7
    8
    9
    |}]
;;
