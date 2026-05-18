open! Base
open Parallel

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let work _ _ = fib 10

module Bench_seqs (Scheduler : Common.Scheduler) = struct
  let parallel = Scheduler.parallel

  let%bench "work-balanced" =
    parallel (fun parallel ->
      let ints = Sequence.init 10_000 ~f:work in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "work-fib" =
    parallel (fun parallel ->
      let ints = Sequence.init 40 ~f:(fun _ i -> fib i) in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-balanced" =
    parallel (fun parallel ->
      let ints = Sequence.range 0 500 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ _ ->
          let ints = Sequence.init 500 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-outer" =
    parallel (fun parallel ->
      let ints = Sequence.range 0 5000 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ _ ->
          let ints = Sequence.init 50 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-inner" =
    parallel (fun parallel ->
      let ints = Sequence.range 0 50 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ _ ->
          let ints = Sequence.init 5000 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-fib" =
    parallel (fun parallel ->
      let ints = Sequence.range 0 30 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ i ->
          let ints = Sequence.init i ~f:(fun _ i -> fib i) in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "pseq_fast_parfor" =
    parallel (fun parallel ->
      let ints = Sequence.range 0 1_000_000 in
      Sequence.iter parallel ints ~f:(fun _ _ -> ()) [@nontail])
  ;;
end

module%bench _ = Common.Bench_schedulers (Bench_seqs)
