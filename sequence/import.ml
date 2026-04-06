open! Base
module Option_u = Unboxed_datatypes.Option_u

module Array : sig @@ portable
  include module type of Array

  external unsafe_racy_set_shared
    : ('a : any mod separable).
    ('a t[@local_opt]) @ shared -> int -> 'a -> unit
    = "%array_unsafe_set"
  [@@layout_poly]
end = struct
  include Array

  external unsafe_racy_set_shared
    : ('a : any mod separable).
    ('a t[@local_opt]) @ shared -> int -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
  [@@layout_poly]
end
