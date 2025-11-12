open! Base
open! Import
open Ocaml_simd_avx
module Bigstring = Base_bigstring

module Kind = struct
  type 'a t =
    | Int8 : int8 t
    | Int16 : int16 t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float32 : float32 t
    | Float64 : float t
    | Int8x16 : int8x16 t
    | Int16x8 : int16x8 t
    | Int32x4 : int32x4 t
    | Int64x2 : int64x2 t
    | Float32x4 : float32x4 t
    | Float64x2 : float64x2 t
    | Int8x32 : int8x32 t
    | Int16x16 : int16x16 t
    | Int32x8 : int32x8 t
    | Int64x4 : int64x4 t
    | Float32x8 : float32x8 t
    | Float64x4 : float64x4 t
  [@@deriving sexp_of]

  let[@inline] width (type a) : a t -> int = function
    | Int8 -> 1
    | Int16 -> 2
    | Int32 -> 4
    | Int64 -> 8
    | Float32 -> 4
    | Float64 -> 8
    | Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2 -> 16
    | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float32x8 | Float64x4 -> 32
  ;;
end

type 'a t =
  { kind : 'a Kind.t
  ; data : Bigstring.t
  }
[@@deriving sexp_of]

external length : Bigstring.t @ contended -> int @@ portable = "%caml_ba_dim_1"

let%template[@inline] with_kind_exn kind data =
  if Bigstring.length data % Kind.width kind <> 0
  then invalid_arg "Bigstring length not divisible by element width.";
  { kind; data }
[@@mode m = (uncontended, shared)]
;;

let[@inline] empty kind =
  { kind
  ; data = Portability_hacks.magic_uncontended__promise_deeply_immutable Bigstring.empty
  }
;;

let[@inline] create kind n = { kind; data = Bigstring.create (n * Kind.width kind) }
let[@inline] kind { kind; _ } = kind
let[@inline] data { data; _ } = data

let%template sub_shared (type a) ({ kind; data } : a t) ~pos ~len =
  let width = Kind.width kind in
  let len = width * len in
  let pos = width * pos in
  { data = Stdlib.Bigarray.Array1.sub (Obj.magic_uncontended data) pos len; kind }
[@@mode m = (uncontended, shared, contended)]
;;

let%template[@inline] copy { kind; data } =
  { kind; data = Bigstring.copy (Obj.magic_uncontended data) }
[@@mode m = (uncontended, shared)]
;;

let[@inline] length { kind; data } = length data / Kind.width kind

(* Bigstrings are usually allocated by [malloc], which provides 16-byte alignment.
   However, custom bigstrings or slices of bigstrings may not be aligned, so we must
   use unaligned instructions. Fortunately, they have no penalty when the address
   is aligned, which is the common case for 16-byte operations. *)

let%template[@inline] get (type a) { kind : a Kind.t; data } pos : a =
  let pos = pos * Kind.width kind in
  match kind with
  | Int8 -> (Bigstring.get_int8 [@inlined]) data ~pos |> Int8.of_int
  | Int16 -> (Bigstring.get_int16_le [@inlined]) data ~pos |> Int16.of_int
  | Int32 -> (Bigstring.get_int32_t_le [@inlined]) data ~pos
  | Int64 -> (Bigstring.get_int64_t_le [@inlined]) data ~pos
  | Float32 -> Float32.Bigstring.get data ~pos
  | Float64 -> Int64.float_of_bits ((Bigstring.get_int64_t_le [@inlined]) data ~pos)
  | Int8x16 -> Int8x16.Bigstring.unaligned_get data ~byte:pos |> Int8x16.box
  | Int16x8 -> Int16x8.Bigstring.unaligned_get data ~byte:pos |> Int16x8.box
  | Int32x4 -> Int32x4.Bigstring.unaligned_get data ~byte:pos |> Int32x4.box
  | Int64x2 -> Int64x2.Bigstring.unaligned_get data ~byte:pos |> Int64x2.box
  | Float32x4 -> Float32x4.Bigstring.unaligned_get data ~byte:pos |> Float32x4.box
  | Float64x2 -> Float64x2.Bigstring.unaligned_get data ~byte:pos |> Float64x2.box
  | Int8x32 -> Int8x32.Bigstring.unaligned_get data ~byte:pos |> Int8x32.box
  | Int16x16 -> Int16x16.Bigstring.unaligned_get data ~byte:pos |> Int16x16.box
  | Int32x8 -> Int32x8.Bigstring.unaligned_get data ~byte:pos |> Int32x8.box
  | Int64x4 -> Int64x4.Bigstring.unaligned_get data ~byte:pos |> Int64x4.box
  | Float32x8 -> Float32x8.Bigstring.unaligned_get data ~byte:pos |> Float32x8.box
  | Float64x4 -> Float64x4.Bigstring.unaligned_get data ~byte:pos |> Float64x4.box
;;

let[@inline] set (type a) { kind : a Kind.t; data } pos (a : a) =
  let pos = pos * Kind.width kind in
  match kind with
  | Int8 -> (Bigstring.set_int8_exn [@inlined]) data ~pos (Int8.to_int a)
  | Int16 -> (Bigstring.set_int16_le_exn [@inlined]) data ~pos (Int16.to_int a)
  | Int32 -> (Bigstring.set_int32_t_le [@inlined]) data ~pos a
  | Int64 -> (Bigstring.set_int64_t_le [@inlined]) data ~pos a
  | Float32 -> Float32.Bigstring.set data ~pos a
  | Float64 -> (Bigstring.set_int64_t_le [@inlined]) data ~pos (Int64.bits_of_float a)
  | Int8x16 -> Int8x16.Bigstring.unaligned_set data ~byte:pos (Int8x16.unbox a)
  | Int16x8 -> Int16x8.Bigstring.unaligned_set data ~byte:pos (Int16x8.unbox a)
  | Int32x4 -> Int32x4.Bigstring.unaligned_set data ~byte:pos (Int32x4.unbox a)
  | Int64x2 -> Int64x2.Bigstring.unaligned_set data ~byte:pos (Int64x2.unbox a)
  | Float32x4 -> Float32x4.Bigstring.unaligned_set data ~byte:pos (Float32x4.unbox a)
  | Float64x2 -> Float64x2.Bigstring.unaligned_set data ~byte:pos (Float64x2.unbox a)
  | Int8x32 -> Int8x32.Bigstring.unaligned_set data ~byte:pos (Int8x32.unbox a)
  | Int16x16 -> Int16x16.Bigstring.unaligned_set data ~byte:pos (Int16x16.unbox a)
  | Int32x8 -> Int32x8.Bigstring.unaligned_set data ~byte:pos (Int32x8.unbox a)
  | Int64x4 -> Int64x4.Bigstring.unaligned_set data ~byte:pos (Int64x4.unbox a)
  | Float32x8 -> Float32x8.Bigstring.unaligned_set data ~byte:pos (Float32x8.unbox a)
  | Float64x4 -> Float64x4.Bigstring.unaligned_set data ~byte:pos (Float64x4.unbox a)
;;

let[@inline] unsafe_get (type a) { kind : a Kind.t; data } pos : a =
  let pos = pos * Kind.width kind in
  match kind with
  | Int8 -> (Bigstring.unsafe_get_int8 [@inlined]) data ~pos |> Int8.of_int
  | Int16 -> (Bigstring.unsafe_get_int16_le [@inlined]) data ~pos |> Int16.of_int
  | Int32 -> (Bigstring.unsafe_get_int32_t_le [@inlined]) data ~pos
  | Int64 -> (Bigstring.unsafe_get_int64_t_le [@inlined]) data ~pos
  | Float32 -> Float32.Bigstring.unsafe_get data ~pos
  | Float64 ->
    Int64.float_of_bits ((Bigstring.unsafe_get_int64_t_le [@inlined]) data ~pos)
  | Int8x16 -> Int8x16.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int8x16.box
  | Int16x8 -> Int16x8.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int16x8.box
  | Int32x4 -> Int32x4.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int32x4.box
  | Int64x2 -> Int64x2.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int64x2.box
  | Float32x4 -> Float32x4.Bigstring.unsafe_unaligned_get data ~byte:pos |> Float32x4.box
  | Float64x2 -> Float64x2.Bigstring.unsafe_unaligned_get data ~byte:pos |> Float64x2.box
  | Int8x32 -> Int8x32.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int8x32.box
  | Int16x16 -> Int16x16.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int16x16.box
  | Int32x8 -> Int32x8.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int32x8.box
  | Int64x4 -> Int64x4.Bigstring.unsafe_unaligned_get data ~byte:pos |> Int64x4.box
  | Float32x8 -> Float32x8.Bigstring.unsafe_unaligned_get data ~byte:pos |> Float32x8.box
  | Float64x4 -> Float64x4.Bigstring.unsafe_unaligned_get data ~byte:pos |> Float64x4.box
;;

let[@inline] unsafe_set (type a) { kind : a Kind.t; data } pos (a : a) =
  let pos = pos * Kind.width kind in
  match kind with
  | Int8 -> (Bigstring.unsafe_set_int8 [@inlined]) data ~pos (Int8.to_int a)
  | Int16 -> (Bigstring.unsafe_set_int16_le [@inlined]) data ~pos (Int16.to_int a)
  | Int32 -> (Bigstring.unsafe_set_int32_t_le [@inlined]) data ~pos a
  | Int64 -> (Bigstring.unsafe_set_int64_t_le [@inlined]) data ~pos a
  | Float32 -> Float32.Bigstring.unsafe_set data ~pos a
  | Float64 ->
    (Bigstring.unsafe_set_int64_t_le [@inlined]) data ~pos (Int64.bits_of_float a)
  | Int8x16 -> Int8x16.Bigstring.unsafe_unaligned_set data ~byte:pos (Int8x16.unbox a)
  | Int16x8 -> Int16x8.Bigstring.unsafe_unaligned_set data ~byte:pos (Int16x8.unbox a)
  | Int32x4 -> Int32x4.Bigstring.unsafe_unaligned_set data ~byte:pos (Int32x4.unbox a)
  | Int64x2 -> Int64x2.Bigstring.unsafe_unaligned_set data ~byte:pos (Int64x2.unbox a)
  | Float32x4 ->
    Float32x4.Bigstring.unsafe_unaligned_set data ~byte:pos (Float32x4.unbox a)
  | Float64x2 ->
    Float64x2.Bigstring.unsafe_unaligned_set data ~byte:pos (Float64x2.unbox a)
  | Int8x32 -> Int8x32.Bigstring.unsafe_unaligned_set data ~byte:pos (Int8x32.unbox a)
  | Int16x16 -> Int16x16.Bigstring.unsafe_unaligned_set data ~byte:pos (Int16x16.unbox a)
  | Int32x8 -> Int32x8.Bigstring.unsafe_unaligned_set data ~byte:pos (Int32x8.unbox a)
  | Int64x4 -> Int64x4.Bigstring.unsafe_unaligned_set data ~byte:pos (Int64x4.unbox a)
  | Float32x8 ->
    Float32x8.Bigstring.unsafe_unaligned_set data ~byte:pos (Float32x8.unbox a)
  | Float64x4 ->
    Float64x4.Bigstring.unsafe_unaligned_set data ~byte:pos (Float64x4.unbox a)
;;

module Expert = struct
  let[@inline] unsafe_racy_get_contended t pos = unsafe_get (Obj.magic_uncontended t) pos

  let[@inline] unsafe_racy_set_contended t pos a =
    unsafe_set (Obj.magic_uncontended t) pos a
  ;;
end
