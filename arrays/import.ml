open! Base
module Int8 = Stdlib_stable.Int8
module Int16 = Stdlib_stable.Int16
module Option_u = Unboxed_datatypes.Option_u

module%template Array : sig @@ portable
  include module type of Array

  [@@@kind.default k = base_or_null]

  val length : ('a : k mod separable). 'a t @ contended -> int
  val unsafe_create_uninitialized : ('a : k mod non_float). int -> 'a t

  val unsafe_racy_get_contended
    : ('a : k mod separable).
    'a t @ contended -> int -> 'a @ contended

  val unsafe_racy_set_contended
    : ('a : k mod separable).
    'a t @ contended -> int -> 'a -> unit

  val set : ('a : k mod separable). 'a t -> int -> 'a -> unit
  val unsafe_set : ('a : k mod separable). 'a t -> int -> 'a -> unit

  [@@@mode.default m = (uncontended, shared)]

  val copy : ('a : k mod separable). 'a t @ m -> 'a t @ m
  val get : ('a : k mod separable). 'a t @ m -> int -> 'a @ m
  val unsafe_get : ('a : k mod separable). 'a t @ m -> int -> 'a @ m
end = struct
  include Array

  let[@inline] unsafe_create_uninitialized len =
    (* It's possible to use this safely because ('a : value mod non_float) *)
    create ~len (Obj.magic None)
  ;;

  external unsafe_create_uninitialized
    :  int
    -> ('a : bits32 mod non_float) t
    @@ portable
    = "caml_make_unboxed_int32_vect_bytecode" "caml_make_unboxed_int32_vect"
  [@@kind bits32]

  external unsafe_create_uninitialized
    :  int
    -> ('a : bits64 mod non_float) t
    @@ portable
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"
  [@@kind bits64]

  external unsafe_create_uninitialized
    :  int
    -> ('a : float32 mod non_float) t
    @@ portable
    = "caml_make_unboxed_float32_vect_bytecode" "caml_make_unboxed_float32_vect"
  [@@kind float32]

  external unsafe_create_uninitialized
    :  int
    -> ('a : float64 mod non_float) t
    @@ portable
    = "caml_floatarray_create"
  [@@kind float64]

  external unsafe_create_uninitialized
    :  int
    -> ('a : word mod non_float) t
    @@ portable
    = "caml_make_unboxed_nativeint_vect_bytecode" "caml_make_unboxed_nativeint_vect"
  [@@kind word]

  external unsafe_racy_get_contended
    : ('a : any mod separable).
    'a t @ contended -> int -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"
  [@@layout_poly]

  external unsafe_racy_set_contended
    : ('a : any mod separable).
    'a t @ contended -> int -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
  [@@layout_poly]

  [@@@kind.default k = base_or_null]

  let length = length
  let set = set
  let unsafe_set = unsafe_set
  let unsafe_racy_get_contended = unsafe_racy_get_contended
  let unsafe_racy_set_contended = unsafe_racy_set_contended

  [@@@mode.default m = (uncontended, shared)]

  let get = (get [@mode m])
  let unsafe_get = (unsafe_get [@mode m])
  let[@inline] copy t = (copy [@kind k]) (Obj.magic_uncontended t)
end

module%template Iarray : sig @@ portable
  include module type of Iarray

  [@@@kind.default k = base_or_null]

  val length : ('a : k mod separable). 'a t @ contended -> int

  val unsafe_racy_get_contended
    : ('a : k mod separable).
    'a t @ contended -> int -> 'a @ contended

  [@@@mode.default m = (uncontended, shared)]

  val get : ('a : k mod separable). 'a t @ m -> int -> 'a @ m
  val unsafe_get : ('a : k mod separable). 'a t @ m -> int -> 'a @ m

  val unsafe_to_array__promise_no_mutation
    : ('a : k mod separable).
    'a t @ m -> 'a array @ m

  val unsafe_of_array__promise_no_mutation
    : ('a : k mod separable).
    'a array @ m -> 'a t @ m
end = struct
  include Iarray

  external unsafe_racy_get_contended
    : ('a : any mod separable).
    'a t @ contended -> int -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"
  [@@layout_poly]

  [@@@kind.default k = base_or_null]

  let length = length
  let unsafe_racy_get_contended = unsafe_racy_get_contended

  [@@@mode.default m = (uncontended, shared)]

  let get = (get [@mode m])
  let unsafe_get = (unsafe_get [@mode m])

  let unsafe_to_array__promise_no_mutation =
    (unsafe_to_array__promise_no_mutation [@mode m])
  ;;

  let unsafe_of_array__promise_no_mutation =
    (unsafe_of_array__promise_no_mutation [@mode m])
  ;;
end
