open! Base
open Parallel
open Unboxed_datatypes

(* Only Arrays are benchmarked as all array types are essentially equivalent. *)
module Array = Arrays.Array

let random =
  let array = Base.Array.init Env.length ~f:(fun _ -> Random.int Env.length) in
  fun () -> Base.Array.copy array
;;

let%bench_fun "sort_base" =
  let array = random () in
  fun () -> Base.Array.sort array ~compare:Int.compare
;;

let%bench_fun "stable_sort_base" =
  let array = random () in
  fun () -> Base.Array.stable_sort array ~compare:Int.compare
;;

module I64 = struct
  type t = int64#

  let to_int = Int64_u.to_int_exn
  let of_int = Int64_u.of_int
end

module Bench_arrays (Scheduler : Common.Scheduler) = struct
  let parallel = Scheduler.parallel

  module%template Layout (Elem : sig
    @@ portable
      type t : k mod contended non_float portable

      val to_int : t @ local -> int
      val of_int : int -> t
    end) =
  struct
    let random =
      let random =
        (Base.Array.init [@kind k]) Env.length ~f:(fun _ ->
          Elem.of_int (Random.int Env.length))
      in
      fun () -> (Base.Array.copy [@kind k]) random
    ;;

    let%bench_fun "init" =
      fun () ->
      Scheduler.parallel (fun parallel ->
        let _ : Elem.t Array.t =
          (Array.init [@kind k]) parallel Env.length ~f:(fun _ i -> Elem.of_int (i * 2))
        in
        ())
    ;;

    let%bench_fun "iter" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.iter [@kind k]) parallel array ~f:(fun _ _ -> ()))
    ;;

    let%bench_fun "fold" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : int =
            (Array.fold [@kind k])
              parallel
              array
              ~init:(fun () -> 0)
              ~f:(fun _ acc i -> acc + Elem.to_int i)
              ~combine:(fun _ a b -> a + b)
          in
          ())
    ;;

    let%bench_fun "find" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : (Elem.t Option_u.t[@kind k]) =
            (Array.find [@kind k]) parallel array ~f:(fun _ i ->
              Elem.to_int i = Random.int Env.length)
          in
          ())
    ;;

    let%bench_fun "map" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : Elem.t Array.t =
            (Array.map [@kind k k]) parallel array ~f:(fun _ i ->
              Elem.of_int (Elem.to_int i * 2))
          in
          ())
    ;;

    let%bench_fun "sort" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : Elem.t Array.t =
            (Array.sort [@kind k]) parallel array ~compare:(fun _ x y ->
              Int.compare (Elem.to_int x) (Elem.to_int y))
          in
          ())
    ;;

    let%bench_fun "stable_sort" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : Elem.t Array.t =
            (Array.stable_sort [@kind k]) parallel array ~compare:(fun _ x y ->
              Int.compare (Elem.to_int x) (Elem.to_int y))
          in
          ())
    ;;

    let%bench_fun "scan" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : #(Elem.t Array.t * Elem.t) =
            (Array.scan [@kind k]) parallel array ~init:(Elem.of_int 0) ~f:(fun _ a b ->
              Elem.of_int (Elem.to_int a + Elem.to_int b))
          in
          ())
    ;;

    let%bench_fun "scan_inclusive" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : Elem.t Array.t =
            (Array.scan_inclusive [@kind k])
              parallel
              array
              ~init:(Elem.of_int 0)
              ~f:(fun _ a b -> Elem.of_int (Elem.to_int a + Elem.to_int b))
          in
          ())
    ;;

    let%bench_fun "filter" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          let _ : Elem.t Array.t =
            (Array.filter [@kind k]) parallel array ~f:(fun _ i ->
              Elem.to_int i >= 500_000)
          in
          ())
    ;;

    let%bench_fun "map_inplace" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.map_inplace [@kind k]) parallel array ~f:(fun _ i ->
            Elem.of_int (Elem.to_int i * 2)))
    ;;

    let%bench_fun "sort_inplace" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.sort_inplace [@kind k]) parallel array ~compare:(fun _ x y ->
            Int.compare (Elem.to_int x) (Elem.to_int y)))
    ;;

    let%bench_fun "stable_sort_inplace" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.stable_sort_inplace [@kind k]) parallel array ~compare:(fun _ x y ->
            Int.compare (Elem.to_int x) (Elem.to_int y)))
    ;;

    let%bench_fun "scan_inplace" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.scan_inplace [@kind k])
            parallel
            array
            ~init:(Elem.of_int 0)
            ~f:(fun _ a b -> Elem.of_int (Elem.to_int a + Elem.to_int b))
          |> (ignore : Elem.t -> unit))
    ;;

    let%bench_fun "scan_inclusive_inplace" =
      let array = random () in
      fun () ->
        parallel (fun parallel ->
          let array = Obj.magic_uncontended array |> Array.of_array in
          (Array.scan_inclusive_inplace [@kind k])
            parallel
            array
            ~init:(Elem.of_int 0)
            ~f:(fun _ a b -> Elem.of_int (Elem.to_int a + Elem.to_int b)))
    ;;
  end
  [@@kind k = (value_or_null, bits64)]

  module _ = Layout (Int)
  module _ = Layout [@kind bits64] (I64)

  let%bench_fun "filter_map" =
    let array = random () in
    fun () ->
      parallel (fun parallel ->
        let array = Obj.magic_uncontended array |> Array.of_array in
        let _ : int Array.t =
          Array.filter_map parallel array ~f:(fun _ i ->
            if i >= 500_000 then This i else Null)
        in
        ())
  ;;
end

module%bench _ = Common.Bench_schedulers (Bench_arrays)
