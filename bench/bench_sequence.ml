open! Base
open Parallel

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let work _ = fib 10

module Bench_seqs (Scheduler : sig
    include Parallel.Scheduler.S

    val configure : 'k create_fn -> 'k
  end) =
struct
  let monitor = Parallel.Monitor.create_root ()
  let scheduler = Scheduler.configure (Scheduler.create [@alert "-experimental"]) ()

  let%bench "work-balanced" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.init 10_000 ~f:work in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "work-fib" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.init 40 ~f:fib in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-balanced" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.range 0 500 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ ->
          let ints = Sequence.init 500 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-outer" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.range 0 5000 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ ->
          let ints = Sequence.init 50 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-inner" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.range 0 50 in
      let ints =
        Sequence.concat_map ints ~f:(fun _ ->
          let ints = Sequence.init 5000 ~f:work in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "concat-fib" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.range 0 30 in
      let ints =
        Sequence.concat_map ints ~f:(fun i ->
          let ints = Sequence.init i ~f:fib in
          Sequence.globalize ints [@nontail])
      in
      let _ : _ = Sequence.to_iarray parallel ints in
      ())
  ;;

  let%bench "pseq_fast_parfor" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let ints = Sequence.range 0 1_000_000 in
      Sequence.iter parallel ints ~f:(fun _ -> ()) [@nontail])
  ;;
end

module%bench Bench_sequential = Bench_seqs (struct
    include Parallel.Scheduler.Sequential

    type 'k create_fn = 'k

    let configure create_fn = create_fn
  end)

module%bench Bench_stack = Bench_seqs (struct
    include Parallel_scheduler_stack

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)

module%bench Bench_work_stealing = Bench_seqs (struct
    include Parallel_scheduler_work_stealing

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)
