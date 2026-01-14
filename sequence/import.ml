open! Base
module Option_u = Unboxed_datatypes.Option_u

module Array : sig @@ portable
  include module type of Array

  external unsafe_racy_set_contended
    :  ('a t[@local_opt]) @ contended
    -> int
    -> 'a
    -> unit
    = "%array_unsafe_set"
end = struct
  include Array

  external unsafe_racy_set_contended
    :  ('a t[@local_opt]) @ contended
    -> int
    -> 'a
    -> unit
    @@ portable
    = "%array_unsafe_set"
end
