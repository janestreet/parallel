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
            let promote () = print_endline "promoted" in
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
