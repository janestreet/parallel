open! Core
open! Async
open! Import
open! Await

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

let run_scope ~x ~y concurrent =
  Concurrent.with_scope concurrent () ~f:(fun spawn ->
    Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
      Atomic.set x (fib_par parallel 10));
    Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
      Atomic.set y (fib_par parallel 10)))
;;

let%expect_test "thread" =
  let run ~max_workers =
    let scheduler =
      Parallel_scheduler.scheduler ?max_workers ~on_root:Concurrent_in_thread.scheduler ()
    in
    let x, y = Atomic.make 0, Atomic.make 0 in
    Concurrent_in_thread.with_blocking Terminator.unkillable ~f:(fun concurrent ->
      Parallel_scheduler.concurrent
        scheduler
        (Concurrent.await concurrent)
        (run_scope ~x ~y) [@nontail]);
    printf "%d %d\n" (Atomic.get x) (Atomic.get y)
  in
  run ~max_workers:(Some 1);
  [%expect {| 89 89 |}];
  run ~max_workers:(Some 2);
  [%expect {| 89 89 |}];
  run ~max_workers:None;
  [%expect {| 89 89 |}];
  return ()
;;

let%expect_test "async" =
  let run ~max_workers =
    let scheduler =
      Parallel_scheduler.scheduler
        ?max_workers
        ~on_root:(Concurrent_in_async.scheduler ())
        ()
    in
    let x, y = Atomic.make 0, Atomic.make 0 in
    let%map () =
      Concurrent_in_async.schedule_with_concurrent
        Terminator.unkillable
        ~f:(fun concurrent ->
          Parallel_scheduler.concurrent
            scheduler
            (Concurrent.await concurrent)
            (run_scope ~x ~y) [@nontail])
    in
    printf "%d %d\n" (Atomic.get x) (Atomic.get y)
  in
  let%bind () = run ~max_workers:(Some 1) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:(Some 2) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:None in
  [%expect {| 89 89 |}];
  return ()
;;

let%expect_test "async spawn_join" =
  let run ~max_workers =
    let scheduler =
      Parallel_scheduler.scheduler
        ?max_workers
        ~on_root:(Concurrent_in_async.scheduler ())
        ()
    in
    let x, y = Atomic.make 0, Atomic.make 0 in
    let%map () =
      Concurrent_in_async.spawn_join scheduler ~f:(fun _ concurrent ->
        run_scope ~x ~y concurrent)
    in
    printf "%d %d\n" (Atomic.get x) (Atomic.get y)
  in
  let%bind () = run ~max_workers:(Some 1) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:(Some 2) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:None in
  [%expect {| 89 89 |}];
  return ()
;;

let%expect_test "async spawn_join (no async worker)" =
  let run ~max_workers =
    let scheduler = Parallel_scheduler.scheduler ?max_workers () in
    let x, y = Atomic.make 0, Atomic.make 0 in
    let%map () =
      Concurrent_in_async.spawn_join scheduler ~f:(fun _ concurrent ->
        run_scope ~x ~y concurrent)
    in
    printf "%d %d\n" (Atomic.get x) (Atomic.get y)
  in
  let%bind () = run ~max_workers:(Some 1) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:(Some 2) in
  [%expect {| 89 89 |}];
  let%bind () = run ~max_workers:None in
  [%expect {| 89 89 |}];
  return ()
;;
