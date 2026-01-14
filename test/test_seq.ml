open! Core
open! Import
open Parallel

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let rec print_list : int list @ contended -> unit = function
  | [] -> print_endline ""
  | [ i ] -> printf "%d\n" i
  | i :: ii ->
    printf "%d " i;
    print_list ii
;;

let print_iarray arr = print_list (Iarray.to_list arr)

let collect parallel seq =
  Sequence.to_list parallel seq |> print_list;
  Sequence.to_iarray parallel seq |> print_iarray;
  print_endline ""
;;

module Test_scheduler (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ()

  module Test_intf (Seq : sig
      include Sequence.S

      val to_seq : 'a t @ local -> 'a Sequence.t @ local @@ portable
    end) =
  struct
    let collect parallel seq = collect parallel (Seq.to_seq seq) [@nontail]

    let%expect_test "empty" =
      Scheduler.parallel scheduler ~f:(fun parallel -> collect parallel Seq.empty);
      [%expect {| |}]
    ;;

    let%expect_test "ints" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        collect parallel (Seq.range 0 15);
        collect
          parallel
          (Seq.range ~stride:3 ~start:`exclusive ~stop:`inclusive 3 18) [@nontail]);
      [%expect
        {|
        0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
        0 1 2 3 4 5 6 7 8 9 10 11 12 13 14

        6 9 12 15 18
        6 9 12 15 18
        |}]
    ;;

    let%expect_test "of_iarray" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        collect parallel (Seq.of_iarray [::]);
        collect parallel (Seq.of_iarray [: 1; 2; 3; 4; 5 :]) [@nontail]);
      [%expect
        {|
        1 2 3 4 5
        1 2 3 4 5
        |}]
    ;;

    let%expect_test "init" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.init 10 ~f:(fun _ i -> i * i) in
        collect parallel ints;
        let ints = Seq.init 10 ~f:(fun _ i -> i % 2) in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 1 4 9 16 25 36 49 64 81
        0 1 4 9 16 25 36 49 64 81

        0 1 0 1 0 1 0 1 0 1
        0 1 0 1 0 1 0 1 0 1
        |}]
    ;;

    let%expect_test "append" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let i0 = Seq.range 0 10 in
        let i1 = Seq.range 0 5 in
        let ints = Seq.append i0 i1 in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
        |}]
    ;;

    let%expect_test "map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 |> Seq.map ~f:(fun _ i -> i * i) in
        collect parallel ints;
        let ints = Seq.range 0 10 |> Seq.map ~f:(fun _ i -> fib i) in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 1 4 9 16 25 36 49 64 81
        0 1 4 9 16 25 36 49 64 81

        1 1 2 3 5 8 13 21 34 55
        1 1 2 3 5 8 13 21 34 55
        |}]
    ;;

    let%expect_test "iter" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        (* Order is non-deterministic *)
        Seq.iter parallel ints ~f:(fun _ _ -> printf ".") [@nontail]);
      [%expect {| .......... |}]
    ;;

    type tree =
      | Empty
      | Leaf of int
      | Node of tree * tree

    let rec print_tree = function
      | Empty -> ()
      | Leaf i -> printf "%d" i
      | Node (l, r) ->
        (* Only print the contents in order; don't reveal the structure of the tree, which
           depends on the choice of fold associativity. *)
        print_tree l;
        print_tree r
    ;;

    let%expect_test "fold" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        Seq.fold
          parallel
          ints
          ~init:(fun () -> "")
          ~f:(fun _ acc i -> acc ^ Int.to_string i)
          ~combine:(fun _ l r -> l ^ r)
        |> print_endline;
        Seq.fold
          parallel
          ints
          ~init:(fun () -> Empty)
          ~f:(fun _ acc i -> Node (acc, Leaf i))
          ~combine:(fun _ l r -> Node (l, r))
        |> print_tree);
      [%expect
        {|
        0123456789
        0123456789
        |}]
    ;;

    let%expect_test "reduce" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        (match Seq.reduce parallel ints ~f:(fun _ acc i -> acc + i) with
         | Some i -> printf "%d\n" i
         | None -> assert false);
        match Seq.reduce parallel ints ~f:(fun _ acc i -> Int.max acc i) with
        | Some i -> printf "%d\n" i
        | None -> assert false);
      [%expect
        {|
        45
        9
        |}]
    ;;

    let%expect_test "find" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        (match Seq.find parallel ints ~f:(fun _ i -> i = 8) with
         | Some i -> printf "%d\n" i
         | None -> assert false);
        match Seq.find parallel ints ~f:(fun _ i -> (i + 1) % 4 = 0) with
        | Some i -> printf "%d\n" i
        | None -> assert false);
      [%expect
        {|
        8
        3
        |}]
    ;;

    let%expect_test "product" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints0 = Seq.range 0 5 in
        let ints1 = Seq.range ~stride:10 0 50 in
        let ints = Seq.product_left ints0 ints1 |> Seq.map ~f:(fun _ (i, j) -> i + j) in
        collect parallel ints;
        let ints = Seq.product_right ints0 ints1 |> Seq.map ~f:(fun _ (i, j) -> i + j) in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 10 20 30 40 1 11 21 31 41 2 12 22 32 42 3 13 23 33 43 4 14 24 34 44
        0 10 20 30 40 1 11 21 31 41 2 12 22 32 42 3 13 23 33 43 4 14 24 34 44

        0 1 2 3 4 10 11 12 13 14 20 21 22 23 24 30 31 32 33 34 40 41 42 43 44
        0 1 2 3 4 10 11 12 13 14 20 21 22 23 24 30 31 32 33 34 40 41 42 43 44
        |}]
    ;;

    let%expect_test "product quickcheck" =
      let check i j =
        Base_quickcheck.Generator.(
          both (int_uniform_inclusive 0 i) (int_uniform_inclusive 0 j))
        |> Expect_test_helpers_core.quickcheck
             ~trials:1000
             ~sexp_of:[%sexp_of: int * int]
             ~f:(fun (i, j) ->
               Scheduler.parallel scheduler ~f:(fun parallel ->
                 let ints0 = Seq.range 0 i in
                 let ints1 = Seq.range 0 j in
                 let ints = Seq.product_left ints0 ints1 in
                 let res = Seq.to_iarray parallel ints in
                 let ref =
                   Iarray.cartesian_product
                     (Iarray.init i ~f:Fn.id)
                     (Iarray.init j ~f:Fn.id)
                 in
                 [%test_eq: (int * int) iarray] res ref))
      in
      check 10 100;
      check 100 10;
      check 50 50
    ;;
  end

  module%test Test_without_length = struct
    module Seq = Sequence

    module%test Test_shared = Test_intf (struct
        include Seq

        let to_seq s = exclave_ s
      end)

    let%expect_test "unfold" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let seq =
          Seq.unfold
            ~init:((0, 10) : int * int)
            ~next:(fun _ (l, r) ->
              if l < r
              then (Option_u.some [@kind value_or_null & value_or_null]) #(l, (l + 1, r))
              else (Option_u.none [@kind value_or_null & value_or_null]) ())
            ~split:(fun _ (l, r) ->
              if l < r - 1
              then
                (Option_u.some [@kind value_or_null & value_or_null])
                  #((l, l + 1), (l + 1, r))
              else (Option_u.none [@kind value_or_null & value_or_null]) ())
        in
        collect parallel seq [@nontail]);
      [%expect
        {|
        0 1 2 3 4 5 6 7 8 9
        0 1 2 3 4 5 6 7 8 9
        |}]
    ;;

    let%expect_test "concat" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints =
          Seq.init 8 ~f:(fun _ i ->
            let ints = Seq.range 0 i in
            Sequence.globalize ints [@nontail])
          |> Seq.concat
        in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6
        0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6
        |}]
    ;;

    let%expect_test "concat_map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints =
          Seq.range 0 8
          |> Seq.concat_map ~f:(fun _ i ->
            let ints = Seq.range 0 i in
            Sequence.globalize ints [@nontail])
        in
        collect parallel ints;
        let ints =
          Seq.range 0 6
          |> Seq.concat_map ~f:(fun _ i ->
            let ints = Seq.range 0 i in
            let ints =
              Seq.concat_map ints ~f:(fun _ i ->
                let ints = Seq.range 0 i in
                Sequence.globalize ints [@nontail])
            in
            Sequence.globalize ints [@nontail])
        in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6
        0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5 0 1 2 3 4 5 6

        0 0 0 1 0 0 1 0 1 2 0 0 1 0 1 2 0 1 2 3
        0 0 0 1 0 0 1 0 1 2 0 0 1 0 1 2 0 1 2 3
        |}]
    ;;

    let%expect_test "filter_map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints =
          Seq.range 0 20
          |> Seq.filter_map ~f:(fun _ i -> if i % 2 = 0 then Some (i / 2) else None)
        in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 1 2 3 4 5 6 7 8 9
        0 1 2 3 4 5 6 7 8 9
        |}]
    ;;

    let concated_ints n = exclave_
      Seq.init n ~f:(fun _ i ->
        let ints = Seq.range i (i + 1) in
        Sequence.globalize ints [@nontail])
      |> Seq.concat
    ;;

    let%expect_test "product of concat" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints0 = concated_ints 5 in
        let ints1 = ints0 |> Seq.map ~f:(fun _ i -> i * 10) in
        let ints = Seq.product_left ints0 ints1 |> Seq.map ~f:(fun _ (i, j) -> i + j) in
        collect parallel ints [@nontail]);
      [%expect
        {|
        0 10 20 30 40 1 11 21 31 41 2 12 22 32 42 3 13 23 33 43 4 14 24 34 44
        0 10 20 30 40 1 11 21 31 41 2 12 22 32 42 3 13 23 33 43 4 14 24 34 44
        |}]
    ;;

    let%expect_test "product of concat quickcheck" =
      let check i j =
        Base_quickcheck.Generator.(
          both (int_uniform_inclusive 0 i) (int_uniform_inclusive 0 j))
        |> Expect_test_helpers_core.quickcheck
             ~trials:250
             ~sexp_of:[%sexp_of: int * int]
             ~f:(fun (i, j) ->
               Scheduler.parallel scheduler ~f:(fun parallel ->
                 let ints0 = concated_ints i in
                 let ints1 = concated_ints j in
                 let ints = Seq.product_left ints0 ints1 in
                 let res = Seq.to_iarray parallel ints in
                 let ref =
                   Iarray.cartesian_product
                     (Iarray.init i ~f:Fn.id)
                     (Iarray.init j ~f:Fn.id)
                 in
                 [%test_eq: (int * int) iarray] res ref))
      in
      check 5 25;
      check 25 5;
      check 25 25
    ;;

    let%expect_test "concat quickcheck" =
      let check i j =
        Base_quickcheck.Generator.(
          both (int_uniform_inclusive 0 i) (int_uniform_inclusive 0 j))
        |> Expect_test_helpers_core.quickcheck
             ~trials:1000
             ~sexp_of:[%sexp_of: int * int]
             ~f:(fun (i, j) ->
               Scheduler.parallel scheduler ~f:(fun parallel ->
                 let ints = Seq.range 0 i in
                 let ints =
                   Seq.concat_map ints ~f:(fun _ _ ->
                     let ints = Seq.init j ~f:(fun _ j -> i * j) in
                     Sequence.globalize ints [@nontail])
                 in
                 let res = Seq.to_iarray parallel ints in
                 let ref =
                   Iarray.concat_map (Iarray.init i ~f:Fn.id) ~f:(fun _ ->
                     Iarray.init j ~f:(fun j -> i * j))
                 in
                 [%test_eq: int iarray] res ref))
      in
      check 10 100;
      check 100 10;
      check 50 50
    ;;
  end

  module%test Test_with_length = struct
    module Seq = Sequence.With_length

    module%test Test_shared = Test_intf (struct
        include Seq

        let to_seq s = exclave_ Sequence.of_with_length s
      end)

    let%expect_test "unfold" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let seq =
          Seq.unfold
            ~init:((0, 10) : int * int)
            ~length:(fun (l, r) -> r - l)
            ~next:(fun _ (l, r) ->
              if l < r
              then (Option_u.some [@kind value_or_null & value_or_null]) #(l, (l + 1, r))
              else (Option_u.none [@kind value_or_null & value_or_null]) ())
            ~split_at:(fun _ (l, r) ~n ->
              if l < r - 1
              then
                (Option_u.some [@kind value_or_null & value_or_null])
                  #((l, l + n), (l + n, r))
              else (Option_u.none [@kind value_or_null & value_or_null]) ())
        in
        printf "%d\n" (Seq.length seq);
        collect parallel (Sequence.of_with_length seq) [@nontail]);
      [%expect
        {|
        10
        0 1 2 3 4 5 6 7 8 9
        0 1 2 3 4 5 6 7 8 9
        |}]
    ;;

    let%expect_test "zip_exn" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints0 = Seq.range 0 10 in
        let ints1 = Seq.range ~stride:2 2 22 in
        let ints = Seq.zip_exn ints0 ints1 |> Seq.map ~f:(fun _ (i, j) -> i + j) in
        collect parallel (Sequence.of_with_length ints) [@nontail]);
      [%expect
        {|
        2 5 8 11 14 17 20 23 26 29
        2 5 8 11 14 17 20 23 26 29
        |}];
      Expect_test_helpers_core.require_does_raise (fun () ->
        let seq = Seq.zip_exn (Seq.range 0 10) Seq.empty in
        Seq.globalize seq [@nontail]);
      [%expect {| (Invalid_argument Parallel_sequence.With_length.zip_exn) |}]
    ;;

    let%expect_test "length" =
      let seq = Seq.range 0 10 in
      printf "%d\n" (Seq.length seq);
      let seq = Seq.of_iarray [: 1; 2; 3; 4; 5; 6 :] in
      printf "%d\n" (Seq.length seq);
      [%expect
        {|
        10
        6
        |}]
    ;;

    let%expect_test "mapi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 |> Seq.mapi ~f:(fun _ i j -> i * j) in
        collect parallel (Sequence.of_with_length ints);
        let ints = Seq.range 0 10 |> Seq.mapi ~f:(fun _ i j -> fib (i + j)) in
        collect parallel (Sequence.of_with_length ints) [@nontail]);
      [%expect
        {|
        0 1 4 9 16 25 36 49 64 81
        0 1 4 9 16 25 36 49 64 81

        1 2 5 13 34 89 233 610 1597 4181
        1 2 5 13 34 89 233 610 1597 4181
        |}]
    ;;

    let%expect_test "iteri" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        (* Order is non-deterministic *)
        Seq.iteri parallel ints ~f:(fun _ _ _ -> printf ".") [@nontail]);
      [%expect {| .......... |}]
    ;;

    let%expect_test "foldi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        Seq.foldi
          parallel
          ints
          ~init:(fun () -> "")
          ~f:(fun _ i acc j -> acc ^ " " ^ Int.to_string i ^ ":" ^ Int.to_string j)
          ~combine:(fun _ l r -> l ^ r)
        |> print_endline);
      [%expect {| 0:0 1:1 2:2 3:3 4:4 5:5 6:6 7:7 8:8 9:9 |}]
    ;;

    let%expect_test "findi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let ints = Seq.range 0 10 in
        (match Seq.findi parallel ints ~f:(fun _ i j -> i = 8 && j = 8) with
         | Some (i, j) -> printf "%d:%d\n" i j
         | None -> assert false);
        match Seq.findi parallel ints ~f:(fun _ i _ -> (i + 1) % 4 = 0) with
        | Some (i, j) -> printf "%d:%d\n" i j
        | None -> assert false);
      [%expect
        {|
        8:8
        3:3
        |}]
    ;;
  end
end

include Common.Test_schedulers (Test_scheduler)
