open! Base
open Parallel
module Array = Arrays.Array

let random =
  let random = Base.Array.init Env.length ~f:(fun _ -> Random.int Env.length) in
  fun () -> Base.Array.copy random
;;

module Bench_arrays (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler =
    (Scheduler.create [@alert "-experimental"]) ~max_domains:Env.max_domains ()
  ;;

  (* Only Arrays are benchmarked as all array types are essentially equivalent. *)

  let%bench_fun "init" =
    fun () ->
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let _ : int Array.t =
        Array.init ~grain:Env.grain parallel Env.length ~f:(fun i -> i * 2)
      in
      ())
  ;;

  let%bench_fun "iter" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.iter ~grain:Env.grain parallel array ~f:(fun _ -> ()))
  ;;

  let%bench_fun "fold" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int =
          Array.fold
            ~grain:Env.grain
            parallel
            array
            ~init:(fun () -> 0)
            ~f:(fun acc i -> acc + i)
            ~combine:(fun a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "find" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int option =
          Array.find ~grain:Env.grain parallel array ~f:(fun i ->
            i = Random.int Env.length)
        in
        ())
  ;;

  let%bench_fun "map" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.map ~grain:Env.grain parallel array ~f:(fun i -> i * 2)
        in
        ())
  ;;

  let%bench_fun "sort" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.sort ~grain:Env.grain parallel array ~compare:Int.compare
        in
        ())
  ;;

  let%bench_fun "stable_sort" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.stable_sort ~grain:Env.grain parallel array ~compare:Int.compare
        in
        ())
  ;;

  let%bench_fun "scan" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t * int =
          Array.scan ~grain:Env.grain parallel array ~init:0 ~f:(fun a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "scan_inclusive" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.scan_inclusive ~grain:Env.grain parallel array ~init:0 ~f:(fun a b ->
            a + b)
        in
        ())
  ;;

  let%bench_fun "filter" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.filter ~grain:Env.grain parallel array ~f:(fun i -> i >= 500_000)
        in
        ())
  ;;

  let%bench_fun "filter_map" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.filter_map ~grain:Env.grain parallel array ~f:(fun i ->
            if i >= 500_000 then This i else Null)
        in
        ())
  ;;

  let%bench_fun "map_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.map_inplace ~grain:Env.grain parallel array ~f:(fun i -> i * 2))
  ;;

  let%bench_fun "sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.sort_inplace ~grain:Env.grain parallel array ~compare:Int.compare)
  ;;

  let%bench_fun "stable_sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.stable_sort_inplace ~grain:Env.grain parallel array ~compare:Int.compare)
  ;;

  let%bench_fun "scan_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.scan_inplace ~grain:Env.grain parallel array ~init:0 ~f:(fun a b -> a + b))
  ;;

  let%bench_fun "scan_inclusive_inplace" =
    let array = random () in
    fun () ->
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.scan_inclusive_inplace
          ~grain:Env.grain
          parallel
          array
          ~init:0
          ~f:(fun a b -> a + b))
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
module%bench Bench_work_stealing = Bench_arrays (Parallel_scheduler_work_stealing)
