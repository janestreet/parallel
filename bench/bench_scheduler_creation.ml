open Base

let max_workers =
  [ 128; 64; 32; 16; 8; 4; 2; 1 ]
  |> List.filter ~f:(fun max_workers -> max_workers <= Domain.recommended_domain_count ())
;;

let%bench ("scheduler creation" [@indexed max_workers = max_workers]) =
  Parallel_scheduler.with_parallel ~max_workers (fun _ -> ())
;;

let%bench "spawn" =
  Concurrent_in_thread.with_blocking Await.Terminator.unkillable ~f:(fun concurrent ->
    Concurrent.spawn_join concurrent () ~f:(fun _ _ _ -> ()))
;;
