open Base
open Async
module Scheduler = Parallel_scheduler

let max_domains = Multicore.max_domains ()
let scheduler = Scheduler.create ~max_domains ()

let%bench_fun "fanout" =
  let async = Parallel_async.create (module Scheduler) scheduler in
  fun () ->
    (* Send one fiber to each worker domain, then wait for the results. This creates an
       equal rate of fiber allocation and deletion. *)
    List.init (max_domains - 1) ~f:(fun _ ->
      Parallel_async.parallel async ~f:(fun _ -> ()))
    |> Deferred.List.all
;;

let%bench_fun ("concurrency" [@indexed fibers = [ 1; 2; 4 ]]) =
  let fibers : int = fibers in
  let async = Parallel_async.create (module Scheduler) scheduler in
  fun () ->
    (* Send one fiber to each worker, which spawns a number of fibers and waits for them. *)
    List.init (max_domains - 1) ~f:(fun _ ->
      Parallel_async.concurrent async ~f:(fun concurrent ->
        Concurrent.with_scope concurrent () ~f:(fun spawn ->
          for _ = 0 to fibers - 1 do
            Concurrent.spawn spawn ~f:(fun _ _ _ -> ())
          done)))
    |> Deferred.List.all
;;
