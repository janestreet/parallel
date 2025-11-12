open! Base
open Parallel
module Array = Arrays.Array

let random =
  let random = Base.Array.init Env.length ~f:(fun _ -> Random.int Env.length) in
  fun () -> Base.Array.copy random
;;

module Bench_arrays (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ~max_domains:Env.max_domains ()

  (* Only Arrays are benchmarked as all array types are essentially equivalent. *)

  let%bench_fun "init" =
    fun () ->
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int Array.t = Array.init parallel Env.length ~f:(fun _ i -> i * 2) in
      ())
  ;;

  let%bench_fun "iter" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.iter parallel array ~f:(fun _ _ -> ()))
  ;;

  let%bench_fun "fold" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int =
          Array.fold
            parallel
            array
            ~init:(fun () -> 0)
            ~f:(fun _ acc i -> acc + i)
            ~combine:(fun _ a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "find" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int option =
          Array.find parallel array ~f:(fun _ i -> i = Random.int Env.length)
        in
        ())
  ;;

  let%bench_fun "map" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t = Array.map parallel array ~f:(fun _ i -> i * 2) in
        ())
  ;;

  let%bench_fun "sort" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.sort parallel array ~compare:(fun _ x y -> Int.compare x y)
        in
        ())
  ;;

  let%bench_fun "stable_sort" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.stable_sort parallel array ~compare:(fun _ x y -> Int.compare x y)
        in
        ())
  ;;

  let%bench_fun "scan" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t * int =
          Array.scan parallel array ~init:0 ~f:(fun _ a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "scan_inclusive" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.scan_inclusive parallel array ~init:0 ~f:(fun _ a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "filter" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t = Array.filter parallel array ~f:(fun _ i -> i >= 500_000) in
        ())
  ;;

  let%bench_fun "filter_map" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.filter_map parallel array ~f:(fun _ i ->
            if i >= 500_000 then This i else Null)
        in
        ())
  ;;

  let%bench_fun "map_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.map_inplace parallel array ~f:(fun _ i -> i * 2))
  ;;

  let%bench_fun "sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.sort_inplace parallel array ~compare:(fun _ x y -> Int.compare x y))
  ;;

  let%bench_fun "stable_sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.stable_sort_inplace parallel array ~compare:(fun _ x y -> Int.compare x y))
  ;;

  let%bench_fun "scan_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.scan_inplace parallel array ~init:0 ~f:(fun _ a b -> a + b))
  ;;

  let%bench_fun "scan_inclusive_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.scan_inclusive_inplace parallel array ~init:0 ~f:(fun _ a b -> a + b))
  ;;
end

let%bench_fun "sort_base" =
  let array = random () in
  fun () -> Base.Array.sort array ~compare:Int.compare
;;

let%bench_fun "stable_sort_base" =
  let array = random () in
  fun () -> Base.Array.stable_sort array ~compare:Int.compare
;;

module%bench Bench_sequential = Bench_arrays (Parallel.Scheduler.Sequential)
module%bench Bench_work_stealing = Bench_arrays (Parallel_scheduler)
