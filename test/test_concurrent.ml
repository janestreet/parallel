open! Core
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

let%expect_test "basic concurrent-parallel" =
  let x, y = Atomic.make 0, Atomic.make 0 in
  Parallel_scheduler.with_concurrent (fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
        Atomic.set x (fib_par parallel 10));
      Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
        Atomic.set y (fib_par parallel 10))));
  printf "%d %d\n" (Atomic.get x) (Atomic.get y);
  [%expect {| 89 89 |}]
;;

let%expect_test "locking in sibling tasks doesn't deadlock" =
  let (P mutex) = Capsule.Sync.Mutex.create () in
  Parallel_scheduler.with_parallel (fun parallel ->
    let #((), ()) =
      Parallel.fork_join2
        parallel
        (fun parallel ->
          Parallel.Capsule.Mutex.with_lock parallel mutex ~f:(fun parallel _ ->
            let #((), ()) =
              Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
            in
            ())
          [@nontail])
        (fun parallel ->
          Parallel.Capsule.Mutex.with_lock parallel mutex ~f:(fun parallel _ ->
            let #((), ()) =
              Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
            in
            ())
          [@nontail])
    in
    ());
  [%expect {| .... |}]
;;

let%expect_test "locking in async tasks doesn't deadlock" =
  let (P mutex) = Capsule.Sync.Mutex.create () in
  Parallel_scheduler.with_parallel ~max_workers:1 (fun parallel ->
    Parallel.Capsule.Mutex.with_lock parallel mutex ~f:(fun _ _ -> printf "."));
  Parallel_scheduler.with_parallel ~max_workers:1 (fun parallel ->
    Parallel.Capsule.Mutex.with_lock parallel mutex ~f:(fun _ _ -> printf "."));
  Parallel_scheduler.with_parallel ~max_workers:1 (fun parallel ->
    Parallel.Capsule.Mutex.with_lock parallel mutex ~f:(fun parallel _ ->
      let #((), ()) =
        Parallel.fork_join2 parallel (fun _ -> printf ".") (fun _ -> printf ".")
      in
      ())
    [@nontail]);
  [%expect {| .... |}]
;;
