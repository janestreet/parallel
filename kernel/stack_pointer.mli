@@ portable

open! Base
open! Import

(** An ['a t] is a raw pointer to an OCaml value of type ['a], but NOT exposed to the GC.
    This allows us to have pointers from one locals stack into another, or even from the
    heap into a locals stack. This is very unsafe! We must exercise extreme caution that
    pointees will be live _and not moved by the GC_ for the pointer's lifetime. *)
type 'a t : word mod everything

val null : unit -> 'a t
val equal : 'a t @ local -> 'b t @ local -> bool

(** [use t ~f] unwraps [t] and passes the result to [f]. The resulting local reference to
    [a] must not outlive the value pointed to by [t]. *)
val%template use
  :  'a t
  -> f:('a option @ local once -> ('b : k) @ local portable unique) @ local once
  -> 'b @ local portable unique
[@@kind k = (value, word & value & value)]

(** [unsafe_with_value a ~f] is only permitted when [a] is stack-allocated and will
    outlive the resulting pointer. Ensures that [a] lives at least as long as the duration
    of [f]. *)
val unsafe_with_value
  :  'a @ local once
  -> f:('a t -> 'b @ local portable unique) @ local once
  -> 'b @ local portable unique

module Imm : sig
  type 'a ptr := 'a t

  (** An ['a t] is a stack pointer encoded as an immediate by setting the bottom bit. *)
  type 'a t : immediate

  val null : 'a t
  val of_ptr : 'a ptr -> 'a t
  val to_ptr : 'a t -> 'a ptr
end
