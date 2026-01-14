open! Base
open! Import
module Bigstring = Base_bigstring

module Kind = struct
  type 'a t =
    | Char : char t
    | Int8 : int8 t
    | Int16 : int16 t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float32 : float32 t
    | Float64 : float t
  [@@deriving sexp_of]

  let width (type a) : a t -> int = function
    | Char -> 1
    | Int8 -> 1
    | Int16 -> 2
    | Int32 -> 4
    | Int64 -> 8
    | Float32 -> 4
    | Float64 -> 8
  [@@inline]
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

let[@inline] of_string ?pos ?len s =
  { kind = Char; data = Bigstring.of_string ?pos ?len s }
;;

let[@inline] length { kind; data } = length data / Kind.width kind

(* Bigstrings are usually allocated by [malloc], which provides 16-byte alignment.
   However, custom bigstrings or slices of bigstrings may not be aligned, so we must use
   unaligned instructions. Fortunately, they have no penalty when the address is aligned,
   which is the common case for 16-byte operations. *)

[%%template
  let get (type a) { kind : a Kind.t; data } pos : a =
    let pos = pos * Kind.width kind in
    match kind with
    | Char -> Bigstring.get data pos
    | Int8 -> (Bigstring.get_int8 [@inlined]) data ~pos |> Int8.of_int
    | Int16 -> (Bigstring.get_int16_le [@inlined]) data ~pos |> Int16.of_int
    | Int32 -> (Bigstring.get_int32_t_le [@inlined]) data ~pos
    | Int64 -> (Bigstring.get_int64_t_le [@inlined]) data ~pos
    | Float32 -> Float32.Bigstring.get data ~pos
    | Float64 -> Int64.float_of_bits ((Bigstring.get_int64_t_le [@inlined]) data ~pos)
  [@@inline]
  ;;]

let set (type a) { kind : a Kind.t; data } pos (a : a) =
  let pos = pos * Kind.width kind in
  match kind with
  | Char -> Bigstring.set data pos a
  | Int8 -> (Bigstring.set_int8_exn [@inlined]) data ~pos (Int8.to_int a)
  | Int16 -> (Bigstring.set_int16_le_exn [@inlined]) data ~pos (Int16.to_int a)
  | Int32 -> (Bigstring.set_int32_t_le [@inlined]) data ~pos a
  | Int64 -> (Bigstring.set_int64_t_le [@inlined]) data ~pos a
  | Float32 -> Float32.Bigstring.set data ~pos a
  | Float64 -> (Bigstring.set_int64_t_le [@inlined]) data ~pos (Int64.bits_of_float a)
[@@inline]
;;

let unsafe_get (type a) { kind : a Kind.t; data } pos : a =
  let pos = pos * Kind.width kind in
  match kind with
  | Char -> Bigstring.unsafe_get data pos
  | Int8 -> (Bigstring.unsafe_get_int8 [@inlined]) data ~pos |> Int8.of_int
  | Int16 -> (Bigstring.unsafe_get_int16_le [@inlined]) data ~pos |> Int16.of_int
  | Int32 -> (Bigstring.unsafe_get_int32_t_le [@inlined]) data ~pos
  | Int64 -> (Bigstring.unsafe_get_int64_t_le [@inlined]) data ~pos
  | Float32 -> Float32.Bigstring.unsafe_get data ~pos
  | Float64 ->
    Int64.float_of_bits ((Bigstring.unsafe_get_int64_t_le [@inlined]) data ~pos)
[@@inline]
;;

let unsafe_set (type a) { kind : a Kind.t; data } pos (a : a) =
  let pos = pos * Kind.width kind in
  match kind with
  | Char -> Bigstring.unsafe_set data pos a
  | Int8 -> (Bigstring.unsafe_set_int8 [@inlined]) data ~pos (Int8.to_int a)
  | Int16 -> (Bigstring.unsafe_set_int16_le [@inlined]) data ~pos (Int16.to_int a)
  | Int32 -> (Bigstring.unsafe_set_int32_t_le [@inlined]) data ~pos a
  | Int64 -> (Bigstring.unsafe_set_int64_t_le [@inlined]) data ~pos a
  | Float32 -> Float32.Bigstring.unsafe_set data ~pos a
  | Float64 ->
    (Bigstring.unsafe_set_int64_t_le [@inlined]) data ~pos (Int64.bits_of_float a)
[@@inline]
;;

module Expert = struct
  let[@inline] unsafe_racy_get_contended t pos = unsafe_get (Obj.magic_uncontended t) pos

  let[@inline] unsafe_racy_set_contended t pos a =
    unsafe_set (Obj.magic_uncontended t) pos a
  ;;
end
