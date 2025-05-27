open! Base
open Parallel
module Array = Arrays.Array

let random =
  let random = Base.Array.init Env.length ~f:(fun _ -> Random.int Env.length) in
  fun () -> Base.Array.copy random
;;

module Bench_arrays (Scheduler : sig
    include Parallel.Scheduler.S

    val configure : 'k create_fn -> 'k
  end) =
struct
  let monitor = Parallel.Monitor.create_root ()
  let scheduler = Scheduler.configure (Scheduler.create [@alert "-experimental"]) ()

  (* Only Arrays are benchmarked as all array types are essentially equivalent. *)

  let%bench_fun "init" =
    fun () ->
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let _ : int Array.t =
        Array.init ~grain:Env.grain parallel Env.length ~f:(fun i -> i * 2)
      in
      ())
  ;;

  let%bench_fun "iter" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.iter ~grain:Env.grain parallel array ~f:(fun _ -> ()))
  ;;

  let%bench_fun "fold" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int =
          Array.fold
            ~grain:Env.grain
            parallel
            array
            ~init:0
            ~f:(fun acc i -> acc + i)
            ~combine:(fun a b -> a + b)
        in
        ())
  ;;

  let%bench_fun "find" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
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
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.map ~grain:Env.grain parallel array ~f:(fun i -> i * 2)
        in
        ())
  ;;

  let%bench_fun "sort" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.sort ~grain:Env.grain parallel array ~compare:Int.compare
        in
        ())
  ;;

  let%bench_fun "stable_sort" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.stable_sort ~grain:Env.grain parallel array ~compare:Int.compare
        in
        ())
  ;;

  let%bench_fun "map_inplace" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.map_inplace ~grain:Env.grain parallel array ~f:(fun i -> i * 2))
  ;;

  let%bench_fun "sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.sort_inplace ~grain:Env.grain parallel array ~compare:Int.compare)
  ;;

  let%bench_fun "stable_sort_inplace" =
    let array = random () in
    fun () ->
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        Array.stable_sort_inplace ~grain:Env.grain parallel array ~compare:Int.compare)
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

module%bench Bench_sequential = Bench_arrays (struct
    include Parallel.Scheduler.Sequential

    type 'k create_fn = 'k

    let configure create_fn = create_fn
  end)

module%bench Bench_stack = Bench_arrays (struct
    include Parallel_scheduler_stack

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)

module%bench Bench_work_stealing = Bench_arrays (struct
    include Parallel_scheduler_work_stealing

    type 'k create_fn = ?domains:int -> 'k

    let configure create_fn = create_fn ?domains:(Some Env.domains)
  end)
