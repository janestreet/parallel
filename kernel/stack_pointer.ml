open! Base
open! Import

(* We don't use [Ocaml_intrinsics.Native_pointer] because we want this library to be JSOO-
   compatible. JS will only use the sequential scheduler, which doesn't need stack
   pointers, and thus we can implement the primitives below by simply raising, as they
   should never be called. *)

type 'a t = nativeint#

external to_nativeint : _ t -> nativeint @@ portable = "%box_nativeint"

external unsafe_of_value
  :  'a @ local once
  -> 'a t @ once
  @@ portable
  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external unsafe_to_value
  :  'a t @ once
  -> 'a @ once
  @@ portable
  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let[@inline] null () = #0n
let[@inline] equal a b = Nativeint.equal (to_nativeint a) (to_nativeint b)

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
[@@kind k = (value, word)]
;;
