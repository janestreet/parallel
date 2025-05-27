open! Base
module Capsule = Portable.Capsule.Expert

let monitor = Parallel.Monitor.create_root ()

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let work () =
  let sum = ref 0 in
  for _ = 1 to 10_000 do
    sum := !sum + fib 10
  done;
  !sum
;;

let work2 parallel =
  let x, y = Parallel.fork_join2 parallel (fun _ -> work ()) (fun _ -> work ()) in
  x + y
;;

let work3 parallel =
  let x, y, z =
    Parallel.fork_join3 parallel (fun _ -> work ()) (fun _ -> work ()) (fun _ -> work ())
  in
  x + y + z
;;

let work4 parallel =
  let x, y, z, w =
    Parallel.fork_join4
      parallel
      (fun _ -> work ())
      (fun _ -> work ())
      (fun _ -> work ())
      (fun _ -> work ())
  in
  x + y + z + w
;;

let work5 parallel =
  let x, y, z, a, b =
    Parallel.fork_join5
      parallel
      (fun _ -> work ())
      (fun _ -> work ())
      (fun _ -> work ())
      (fun _ -> work ())
      (fun _ -> work ())
  in
  x + y + z + a + b
;;

(* Spawns a tree of tasks with 2^n leaves that run [work]. *)
let rec work_tree parallel n =
  match n with
  | 0 -> work ()
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> work_tree parallel (n - 1))
        (fun parallel -> work_tree parallel (n - 1))
    in
    a + b
;;

(* Spawns a tree of tasks with 2^n leaves that do nothing. *)
let rec fast_tree parallel n =
  match n with
  | 0 -> 1
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> fast_tree parallel (n - 1))
        (fun parallel -> fast_tree parallel (n - 1))
    in
    a + b
;;

let rec fast_tree3 parallel n =
  match n with
  | 0 -> 1
  | n ->
    let a, b, c =
      Parallel.fork_join3
        parallel
        (fun parallel -> fast_tree3 parallel (n - 1))
        (fun parallel -> fast_tree3 parallel (n - 1))
        (fun parallel -> fast_tree3 parallel (n - 1))
    in
    a + b + c
;;

let rec fast_tree_seq_unmonitored = function
  | 0 -> 1
  | n -> fast_tree_seq_unmonitored (n - 1) + fast_tree_seq_unmonitored (n - 1)
;;

let rec fast_tree_seq_monitored = function
  | 0 -> 1
  | n ->
    (match
       ( Parallel.Panic.Result.handle_panics_and_report_exceptions monitor (fun () ->
           fast_tree_seq_monitored (n - 1))
       , Parallel.Panic.Result.handle_panics_and_report_exceptions monitor (fun () ->
           fast_tree_seq_monitored (n - 1)) )
     with
     | Ok (a, _), Ok (b, _) -> Capsule.Data.project a + Capsule.Data.project b
     | _ -> assert false)
;;

let rec par_fib parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let a, b =
      Parallel.fork_join2
        parallel
        (fun parallel -> par_fib parallel (n - 1))
        (fun parallel -> par_fib parallel (n - 2))
    in
    a + b
;;

let for_ ~f ~start ~stop =
  for i = start to stop - 1 do
    match
      Parallel.Panic.Result.handle_panics_and_report_exceptions monitor (fun () -> f i)
    with
    | Ok _ -> ()
    | _ -> assert false
  done
;;

let rec for_forkjoin parallel ~f ~start ~stop =
  if start >= stop
  then ()
  else (
    let pivot = start + ((stop - start) / 2) in
    if pivot = start
    then f start
    else (
      let (), () =
        Parallel.fork_join2
          parallel
          (fun parallel -> for_forkjoin parallel ~f ~start ~stop:pivot)
          (fun parallel -> for_forkjoin parallel ~f ~start:pivot ~stop)
      in
      ()))
;;

module Bench_parallel (Scheduler : sig
    include Parallel.Scheduler.S

    val configure : 'k create_fn -> 'k
  end) =
struct
  let monitor = Parallel.Monitor.create_root ()
  let scheduler = Scheduler.configure (Scheduler.create [@alert "-experimental"]) ()

  let%bench "work2" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = work2 parallel in
      ())
  ;;

  let%bench "work3" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = work3 parallel in
      ())
  ;;

  let%bench "work4" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = work4 parallel in
      ())
  ;;

  let%bench "work5" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = work5 parallel in
      ())
  ;;

  let%bench ("work_tree" [@indexed n = [ 4; 8; 10 ]]) =
    let n : int = n in
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = work_tree parallel n in
      ())
  ;;

  let%bench ("par_fib" [@indexed n = [ 4; 8; 10 ]]) =
    let n : int = n in
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = par_fib parallel n in
      ())
  ;;

  (* [n = 14] chosen so the benchmarks take some hundreds of us each, since the default
     heartbeat interval is 100us. *)
  let%bench "fast_tree" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = fast_tree parallel 14 in
      ())
  ;;

  (* [n = 10] chosen since 3^n grows faster than 2^n.*)
  let%bench "fast_tree3" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int = fast_tree3 parallel 10 in
      ())
  ;;

  let%bench "slow_parfor_forkjoin" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      for_forkjoin
        parallel
        ~f:(fun _ ->
          let _ : int = work () in
          ())
        ~start:0
        ~stop:100)
  ;;

  let%bench "fast_parfor_forkjoin" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      for_forkjoin parallel ~f:(fun _ -> ()) ~start:0 ~stop:1_000_000)
  ;;

  let%bench "fast_parfor" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      Parallel.for_ parallel ~f:(fun _ _ -> ()) ~start:0 ~stop:1_000_000)
  ;;

  let%bench "slow_parfor" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      Parallel.for_
        parallel
        ~f:(fun _ _ ->
          let _ : int = work () in
          ())
        ~start:0
        ~stop:100)
  ;;

  let%bench "forkjoin_in_parfor" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      Parallel.for_
        parallel
        ~f:(fun parallel _ ->
          let _ : int = fast_tree parallel 4 in
          ())
        ~start:0
        ~stop:10_000)
  ;;
end

let%bench "fast_tree_seq_unmonitored" = fast_tree_seq_unmonitored 14
let%bench "fast_tree_seq_monitored" = fast_tree_seq_monitored 14
let%bench "fast_for_seq" = for_ ~f:(fun _ -> ()) ~start:0 ~stop:1_000_000

let%bench "slow_for_seq" =
  for_
    ~f:(fun _ ->
      let _ : int = work () in
      ())
    ~start:0
    ~stop:100
;;

module%bench Bench_sequential = Bench_parallel (struct
    include Parallel.Scheduler.Sequential

    type 'k create_fn = 'k

    let configure create_fn = create_fn
  end)

module%bench Bench_stack = Bench_parallel (struct
    include Parallel_scheduler_stack

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)

module%bench Bench_work_stealing = Bench_parallel (struct
    include Parallel_scheduler_work_stealing

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)
