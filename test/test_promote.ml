open! Core
open! Import
module Runqueue = Parallel.For_testing.Runqueue

let%expect_test "max promotions" =
  let q = Runqueue.create () in
  let job (_ : Parallel.t) = () in
  Runqueue.with_jobs q [ job; job; job; job; job ] ~f:(fun q ->
    Runqueue.with_jobs q [ job; job ] ~f:(fun q ->
      Runqueue.with_jobs q [ job ] ~f:(fun q ->
        Runqueue.with_jobs q [ job ] ~f:(fun q ->
          Runqueue.with_jobs q [ job ] ~f:(fun q ->
            let promote ~tokens:_ = print_endline "promoted" in
            (* One batch of five jobs. *)
            Runqueue.promote q ~n:1 ~f:promote;
            [%expect
              {|
              promoted
              promoted
              promoted
              promoted
              promoted
              |}];
            (* One batch of two jobs. *)
            Runqueue.promote q ~n:2 ~f:promote;
            [%expect
              {|
              promoted
              promoted
              |}];
            (* Two batches of one job. *)
            Runqueue.promote q ~n:2 ~f:promote;
            [%expect
              {|
              promoted
              promoted
              |}];
            (* One batch of one job. *)
            Runqueue.promote q ~n:1 ~f:promote;
            [%expect {| promoted |}];
            (* Empty queue. *)
            Runqueue.promote q ~n:3 ~f:promote;
            [%expect {| |}])))))
  [@nontail]
;;

let%expect_test "token distribution" =
  let q = Runqueue.create () in
  let job (_ : Parallel.t) = () in
  let test jobs =
    Runqueue.with_jobs q jobs ~f:(fun q ->
      let promote ~tokens = printf "tokens: %d\n" tokens in
      Runqueue.promote q ~n:19 ~f:promote;
      printf "remaining: %d\n" (Runqueue.tokens q))
  in
  test [ job ];
  [%expect
    {|
    tokens: 9
    remaining: 9
    |}];
  test [ job; job ];
  [%expect
    {|
    tokens: 5
    tokens: 5
    remaining: 7
    |}];
  test [ job; job; job ];
  [%expect
    {|
    tokens: 4
    tokens: 4
    tokens: 4
    remaining: 4
    |}];
  test [ job; job; job; job ];
  [%expect
    {|
    tokens: 3
    tokens: 3
    tokens: 3
    tokens: 3
    remaining: 3
    |}];
  test [ job; job; job; job; job ];
  [%expect
    {|
    tokens: 2
    tokens: 2
    tokens: 2
    tokens: 2
    tokens: 2
    remaining: 4
    |}];
  test [ job; job; job; job; job; job ];
  [%expect
    {|
    tokens: 1
    tokens: 1
    tokens: 1
    tokens: 1
    tokens: 1
    tokens: 1
    remaining: 7
    |}]
;;

let%expect_test "negative tokens" =
  let q = Runqueue.create () in
  let job (_ : Parallel.t) = () in
  Runqueue.with_jobs q [ job; job; job ] ~f:(fun q ->
    let promote ~tokens = printf "tokens: %d\n" tokens in
    Runqueue.promote q ~n:1 ~f:promote;
    printf "remaining: %d\n" (Runqueue.tokens q));
  [%expect
    {|
    tokens: 0
    tokens: 0
    tokens: 0
    remaining: -2
    |}]
;;
