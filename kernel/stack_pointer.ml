open! Base
open! Import

(* We don't use [Ocaml_intrinsics.Native_pointer] because we want this library to be JSOO-
   compatible. JS will only use the sequential scheduler, which doesn't need stack
   pointers, and thus we can implement the primitives below by simply raising, as they
   should never be called. *)

type 'a t = nativeint#

external unsafe_of_value
  :  'a @ local once
  -> 'a t @ once
  @@ portable
  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external unsafe_to_value
  :  'a t
  -> 'a
  @@ portable
  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let null = Nativeint_u.zero
let equal = Nativeint_u.equal

let[@inline] unsafe_with_value (type a) (a : a) ~f = exclave_
  (* Safe because [opaque_identity] does not look at [a]. *)
  let a = Obj.magic_many a in
  let res = f (unsafe_of_value a) in
  (* Ensure that [a] is still live. *)
  let _ : a = Sys.opaque_identity a in
  res
;;

let%template[@inline] use t ~f = exclave_
  f (if equal t (null ()) then None else Some (unsafe_to_value t))
[@@kind k = (value, word & value & value)]
;;

module Imm = struct
  type 'a ptr = 'a t
  type 'a t = int

  let null = 0

  external of_i64
    :  int64#
    -> 'a t
    @@ portable
    = "%reinterpret_unboxed_int64_as_tagged_int63"

  let[@inline] of_ptr ptr = of_i64 (Int64_u.unbox (Nativeint_u.to_int64 ptr))

  external to_ptr
    :  'a t
    -> 'a ptr
    @@ portable
    = "caml_ext_pointer_as_native_pointer_bytecode" "caml_ext_pointer_as_native_pointer"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end
