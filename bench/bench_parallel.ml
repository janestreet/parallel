open! Base

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
  let #(x, y) = Parallel.fork_join2 parallel (fun _ -> work ()) (fun _ -> work ()) in
  x + y
;;

let work3 parallel =
  let #(x, y, z) =
    Parallel.fork_join3 parallel (fun _ -> work ()) (fun _ -> work ()) (fun _ -> work ())
  in
  x + y + z
;;

let work4 parallel =
  let #(x, y, z, w) =
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
  let #(x, y, z, a, b) =
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
    let #(a, b) =
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
    let #(a, b) =
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
    let #(a, b, c) =
      Parallel.fork_join3
        parallel
        (fun parallel -> fast_tree3 parallel (n - 1))
        (fun parallel -> fast_tree3 parallel (n - 1))
        (fun parallel -> fast_tree3 parallel (n - 1))
    in
    a + b + c
;;

let rec fast_tree_seq = function
  | 0 -> 1
  | n -> fast_tree_seq (n - 1) + fast_tree_seq (n - 1)
;;

let rec par_fib parallel n =
  match n with
  | 0 | 1 -> 1
  | n ->
    let #(a, b) =
      Parallel.fork_join2
        parallel
        (fun parallel -> par_fib parallel (n - 1))
        (fun parallel -> par_fib parallel (n - 2))
    in
    a + b
;;

let for_ ~f ~start ~stop =
  for i = start to stop - 1 do
    f i
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
      let #((), ()) =
        Parallel.fork_join2
          parallel
          (fun parallel -> for_forkjoin parallel ~f ~start ~stop:pivot)
          (fun parallel -> for_forkjoin parallel ~f ~start:pivot ~stop)
      in
      ()))
;;

module Bench_parallel (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ~max_domains:Env.max_domains ()

  let%bench "work2" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = work2 parallel in
      ())
  ;;

  let%bench "work3" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = work3 parallel in
      ())
  ;;

  let%bench "work4" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = work4 parallel in
      ())
  ;;

  let%bench "work5" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = work5 parallel in
      ())
  ;;

  let%bench ("work_tree" [@indexed n = [ 4; 8; 10 ]]) =
    let n : int = n in
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = work_tree parallel n in
      ())
  ;;

  let%bench ("par_fib" [@indexed n = [ 4; 8; 10 ]]) =
    let n : int = n in
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = par_fib parallel n in
      ())
  ;;

  (* [n = 16] chosen so each iteration takes about one default heartbeat interval (250us) *)
  let%bench "fast_tree" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = fast_tree parallel 16 in
      ())
  ;;

  (* [n = 10] chosen since 3^n grows faster than 2^n. *)
  let%bench "fast_tree3" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int = fast_tree3 parallel 10 in
      ())
  ;;

  let%bench "slow_parfor_forkjoin" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      for_forkjoin
        parallel
        ~f:(fun _ ->
          let _ : int = work () in
          ())
        ~start:0
        ~stop:100)
  ;;

  let%bench "fast_parfor_forkjoin" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      for_forkjoin parallel ~f:(fun _ -> ()) ~start:0 ~stop:1_000_000)
  ;;

  let%bench "fast_parfor" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel.for_ parallel ~f:(fun _ _ -> ()) ~start:0 ~stop:1_000_000)
  ;;

  let%bench "slow_parfor" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel.for_
        parallel
        ~f:(fun _ _ ->
          let _ : int = work () in
          ())
        ~start:0
        ~stop:100)
  ;;

  let%bench "forkjoin_in_parfor" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel.for_
        parallel
        ~f:(fun parallel _ ->
          let _ : int = fast_tree parallel 4 in
          ())
        ~start:0
        ~stop:10_000)
  ;;

  let%bench "schedule_only" =
    for _ = 1 to 1_000 do
      Scheduler.parallel scheduler ~f:(fun _ -> ())
    done
  ;;

  let%bench "many_fast_parfor" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      for _ = 1 to 100 do
        Parallel.Scheduler.heartbeat parallel ~n:Env.eager;
        Parallel.for_ parallel ~f:(fun _ _ -> ()) ~start:0 ~stop:5_000
      done)
  ;;

  let%bench "many_sched_fast_parfor" =
    for _ = 1 to 100 do
      Scheduler.parallel scheduler ~f:(fun parallel ->
        Parallel.Scheduler.heartbeat parallel ~n:Env.eager;
        Parallel.for_ parallel ~f:(fun _ _ -> ()) ~start:0 ~stop:5_000)
    done
  ;;

  let%bench "unbalanced fork_join3" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let #((), _, _) =
        Parallel.fork_join3 parallel (fun _ -> ()) (fun _ -> work ()) (fun _ -> work ())
      in
      ())
  ;;
end

let%bench "fast_tree_seq" = fast_tree_seq 14
let%bench "fast_for_seq" = for_ ~f:(fun _ -> ()) ~start:0 ~stop:1_000_000

let%bench "slow_for_seq" =
  for_
    ~f:(fun _ ->
      let _ : int = work () in
      ())
    ~start:0
    ~stop:100
;;

module%bench Bench_sequential = Bench_parallel (Parallel.Scheduler.Sequential)
module%bench Bench_work_stealing = Bench_parallel (Parallel_scheduler)
