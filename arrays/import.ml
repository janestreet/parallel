open! Base
module Int8 = Stdlib_stable.Int8
module Int16 = Stdlib_stable.Int16
module Option_u = Unboxed_datatypes.Option_u

module Array : sig @@ portable
  include module type of Array

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  val copy : 'a t @ m -> 'a t @ m]

  val unsafe_racy_get_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a @ contended

  val unsafe_racy_set_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a -> unit
end = struct
  include Array

  let%template[@inline] copy t = copy (Obj.magic_uncontended t)
  [@@mode m = (uncontended, shared)]
  ;;

  external unsafe_racy_get_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"

  external unsafe_racy_set_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
end

module Iarray : sig @@ portable
  include module type of Iarray

  val unsafe_racy_get_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a @ contended
end = struct
  include Iarray

  external unsafe_racy_get_contended
    : ('a : value_or_null mod separable).
    'a t @ contended -> int -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"
end

module Vec : sig @@ portable
  include module type of Vec with type 'a t = 'a Vec.t

  val length : 'a t @ shared -> int

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  val copy : 'a t @ m -> 'a t @ m
  val get : 'a t @ m -> int -> 'a @ m
  val unsafe_get : 'a t @ m -> int -> 'a @ m]

  val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a -> unit
end = struct
  include Vec

  let[@inline] length t = length (Obj.magic_uncontended t)
  let%template[@inline] [@mode shared] get t i = get (Obj.magic_uncontended t) i

  let%template[@inline] [@mode shared] unsafe_get t i =
    unsafe_get (Obj.magic_uncontended t) i
  ;;

  let%template[@inline] [@mode shared] copy t = copy (Obj.magic_uncontended t)
  let[@inline] unsafe_racy_get_contended t i = unsafe_get (Obj.magic_uncontended t) i
  let[@inline] unsafe_racy_set_contended t i a = unsafe_set (Obj.magic_uncontended t) i a
end
