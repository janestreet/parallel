open! Base

open struct
  module Empty_portable (M : sig
    @@ portable
      type 'a t

      val empty : unit -> 'a t
    end) : sig
    @@ portable
    val empty : unit -> 'a M.t @ portable
  end = struct
    let empty = Obj.magic Obj.magic M.empty
  end
end

module Option : sig @@ portable
  include module type of Option

  val first_some_portable_contended
    :  'a t @ contended portable
    -> 'a t @ contended portable
    -> 'a t @ contended portable

  val merge_portable_contended
    :  'a t @ contended portable
    -> 'a t @ contended portable
    -> f:('a @ contended portable -> 'a @ contended portable -> 'a @ contended portable)
       @ local portable
    -> 'a t @ contended portable
end = struct
  include Option

  let[@inline] first_some_portable_contended x y =
    match x with
    | Some _ -> x
    | None -> y
  ;;

  let[@inline] merge_portable_contended a b ~f =
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (f a b)
  ;;
end

module Array : sig @@ portable
  include module type of Array

  val empty : unit -> 'a t @ portable
  val length : 'a t @ contended -> int

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  val copy : 'a t @ m -> 'a t @ m
  val get : 'a t @ m -> int -> 'a @ m
  val unsafe_get : 'a t @ m -> int -> 'a @ m]

  val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a -> unit
end = struct
  include Array

  include Empty_portable (struct
      type 'a t = 'a array

      let empty () = [||]
    end)

  let[@inline] length t = length (Obj.magic_uncontended t)

  let%template[@inline] copy t = copy (Obj.magic_uncontended t)
  [@@mode m = (uncontended, shared)]
  ;;

  let%template[@inline] [@mode shared] get t i = get (Obj.magic_uncontended t) i

  let%template[@inline] [@mode shared] unsafe_get t i =
    unsafe_get (Obj.magic_uncontended t) i
  ;;

  external unsafe_racy_get_contended
    :  'a t @ contended
    -> int
    -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"

  external unsafe_racy_set_contended
    :  'a t @ contended
    -> int
    -> 'a
    -> unit
    @@ portable
    = "%array_unsafe_set"
end

module Iarray : sig @@ portable
  include module type of Iarray

  val empty : unit -> 'a t @ portable
  val length : 'a t @ contended -> int
  val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  val get : 'a t @ m -> int -> 'a @ m
  val unsafe_get : 'a t @ m -> int -> 'a @ m
  val unsafe_to_array__promise_no_mutation : 'a t @ m -> 'a array
  val unsafe_of_array__promise_no_mutation : 'a array @ m -> 'a t]
end = struct
  include Iarray

  include Empty_portable (struct
      type 'a t = 'a iarray

      let empty () = [::]
    end)

  let[@inline] length t = length (Obj.magic_uncontended t)
  let%template[@inline] [@mode shared] get t i = get (Obj.magic_uncontended t) i

  let%template[@inline] [@mode shared] unsafe_get t i =
    unsafe_get (Obj.magic_uncontended t) i
  ;;

  external unsafe_racy_get_contended
    :  'a t @ contended
    -> int
    -> 'a @ contended
    @@ portable
    = "%array_unsafe_get"

  let%template[@inline] [@mode shared] unsafe_to_array__promise_no_mutation t =
    unsafe_to_array__promise_no_mutation (Obj.magic_uncontended t)
  ;;

  let%template[@inline] [@mode shared] unsafe_of_array__promise_no_mutation t =
    unsafe_of_array__promise_no_mutation (Obj.magic_uncontended t)
  ;;
end

module Vec : sig @@ portable
  type 'a t = 'a Vec.t

  val set : 'a t -> int -> 'a -> unit
  val unsafe_set : 'a t -> int -> 'a -> unit
  val empty : unit -> 'a t @ portable
  val length : 'a t @ contended -> int
  val create : len:int -> 'a -> 'a t

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  val copy : 'a t @ m -> 'a t @ m
  val get : 'a t @ m -> int -> 'a @ m
  val unsafe_get : 'a t @ m -> int -> 'a @ m]

  val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a -> unit
end = struct
  module type Vec = Vec.S with type index := int and type 'a t = 'a Vec.t

  module type Vec_portable = sig @@ portable
    include Vec
  end

  include (val Obj.magic (module Vec : Vec) : Vec_portable)

  include Empty_portable (struct
      type 'a t = 'a Vec.t

      let empty () = create ~initial_capacity:0 ()
    end)

  let[@inline] length t = length (Obj.magic_uncontended t)
  let[@inline] create ~len a = init len ~f:(fun _ -> a)
  let%template[@inline] [@mode shared] get t i = get (Obj.magic_uncontended t) i

  let%template[@inline] [@mode shared] unsafe_get t i =
    unsafe_get (Obj.magic_uncontended t) i
  ;;

  let%template[@inline] [@mode shared] copy t = copy (Obj.magic_uncontended t)
  let[@inline] unsafe_racy_get_contended t i = unsafe_get (Obj.magic_uncontended t) i
  let[@inline] unsafe_racy_set_contended t i a = unsafe_set (Obj.magic_uncontended t) i a
end
