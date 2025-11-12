open! Base
open Parallel

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let work _ _ = fib 10

module Bench_seqs (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ~max_domains:Env.max_domains ()

  let%bench "work-balanced" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let ints = Sequence.init 10_000 ~f:work in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "work-fib" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let ints = Sequence.init 40 ~f:(fun _ i -> fib i) in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-balanced" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
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
    Scheduler.parallel scheduler ~f:(fun parallel ->
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
    Scheduler.parallel scheduler ~f:(fun parallel ->
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
    Scheduler.parallel scheduler ~f:(fun parallel ->
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
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let ints = Sequence.range 0 1_000_000 in
      Sequence.iter parallel ints ~f:(fun _ _ -> ()) [@nontail])
  ;;
end

module%bench Bench_sequential = Bench_seqs (Parallel.Scheduler.Sequential)
module%bench Bench_work_stealing = Bench_seqs (Parallel_scheduler)
