@@ portable

open! Base
open! Import

module Kind : sig
  type 'a t =
    | Char : char t
    | Int8 : int8 t
    | Int16 : int16 t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float32 : float32 t
    | Float64 : float t
  [@@deriving sexp_of]

  (** Size of the scalar type in bytes. *)
  val width : 'a t -> int
end

(** Typed bigstring representing an array of scalars. *)
type 'a t = private
  { kind : 'a Kind.t
  ; data : Base_bigstring.t
  }
[@@deriving sexp_of]

(** [with_kind_exn (kind : a Kind.t) data] checks that the length of [data] is divisible
    by the width of [a] and returns a typed [a t]. *)
val%template with_kind_exn : 'a Kind.t -> Base_bigstring.t @ m -> 'a t @ m
[@@mode m = (uncontended, shared)]

(** [empty kind] is an empty typed bigstring containing [kind]s. *)
val empty : 'a Kind.t -> 'a t

(** [create kind n] is an uninitialized typed bigstring containing [n] [kind]s. *)
val create : 'a Kind.t -> int -> 'a t

(** [kind t] is the kind of scalar stored in [t]. *)
val kind : 'a t @ contended -> 'a Kind.t

(** [data t] is the underlying bigstring for [t]. *)
val data : 'a t -> Base_bigstring.t

(** [sub_shared t ~pos ~len] is a bigstring containing [len] elements of [t] beginning
    with the element at index [pos], pointing to the same memory as the input bigstring. *)
val%template sub_shared : 'a t @ m -> pos:int -> len:int -> 'a t @ m
[@@mode m = (uncontended, shared, contended)]

(** [length t] is the length of [t] in scalars. *)
val length : 'a t @ contended -> int

(** [copy t] duplicates [t]. *)
val%template copy : 'a t @ m -> 'a t @ m
[@@mode m = (uncontended, shared)]

(** [of_string s] is a new character bigstring initialized by copying the contents of the
    string [s]. The optional arguments [pos] and [len] are forwarded to
    {!Base_bigstring.of_string} *)
val of_string : ?pos:int -> ?len:int -> string -> char t

(** [get t i] loads the scalar at index [i].

    Raises [Invalid_argument] if [i] is out of bounds. *)
val get : 'a t @ shared -> int -> 'a @ portable

(** [set t i a] stores the scalar [a] to index [i].

    Raises [Invalid_argument] if [i] is out of bounds. *)
val set : 'a t -> int -> 'a @ portable -> unit

(** [unsafe_get t i] loads the scalar at index [i]. Does not check bounds. *)
val unsafe_get : 'a t @ shared -> int -> 'a @ portable

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
