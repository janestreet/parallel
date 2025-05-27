@@ portable

open! Base
open! Import

module Kind : sig
  (** Scalar type stored in the bigstring. *)
  type 'a t =
    | Int8 : Int_repr.int8 t
    | Int16 : Int_repr.int16 t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float32 : float32 t
    | Float64 : float t
  [@@deriving sexp_of]

  (** Size of the scalar type in bytes. *)
  val width : 'a t -> int
end

(** Typed bigstring representing an array of scalars. *)
type 'a t : value mod portable = private
  { kind : 'a Kind.t
  ; data : Base_bigstring.t
  }
[@@deriving sexp_of]

(** [with_kind_exn (kind : a Kind.t) data] checks that the length of [data] is divisible
    by the width of [a] and returns a typed [a t]. *)
val with_kind_exn : 'a Kind.t -> Base_bigstring.t -> 'a t

(** [empty kind] is an empty typed bigstring containing [kind]s. *)
val empty : 'a Kind.t -> 'a t

(** [create kind n] is an uninitialized typed bigstring containing [n] [kind]s. *)
val create : 'a Kind.t -> int -> 'a t

(** [kind t] is the kind of scalar stored in [t]. *)
val kind : 'a t @ contended -> 'a Kind.t

(** [data t] is the underlying bigstring for [t]. *)
val data : 'a t -> Base_bigstring.t

(** [length t] is the length of [t] in scalars. *)
val length : 'a t @ contended -> int

(** [copy t] duplicates [t]. *)
val%template copy : 'a t @ m -> 'a t @ m
[@@mode m = (uncontended, shared)]

(** [get t i] loads the scalar at index [i].

    Raises [Invalid_argument] if [i] is out of bounds. *)
val get : 'a t -> int -> 'a @ portable

(** [set t i a] stores the scalar [a] to index [i].

    Raises [Invalid_argument] if [i] is out of bounds. *)
val set : 'a t -> int -> 'a @ portable -> unit

(** [unsafe_get t i] loads the scalar at index [i]. Does not check bounds. *)
val unsafe_get : 'a t -> int -> 'a @ portable

(** [unsafe_set t i a] stores the scalar [a] to index [i]. Does not check bounds. *)
val unsafe_set : 'a t -> int -> 'a @ portable -> unit

module Expert : sig
  (** [unsafe_racy_get_contended t i] loads the scalar at index [i] from a contended [t].
      Does not check bounds. *)
  val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable

  (** [unsafe_racy_set_contended t i a] writes the scalar [a] to index [i] in a contended
      [t]. Does not check bounds. *)
  val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a @ portable -> unit
end
