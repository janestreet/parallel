open! Base
open! Import

module type Get = sig @@ portable
  type 'a t

  (** [length t] returns the number of elements in [t]. *)
  val length : 'a t @ contended -> int

  (** [get t i] reads the element at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val get : 'a t @ portable -> int -> 'a @ portable

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get : 'a t @ portable -> int -> 'a @ portable
end

module type Set = sig @@ portable
  type 'a t

  (** To store a value in a parallel array, we must prove that it does not share
      unsynchronized state with any other elements. This is the case if its type crosses
      contention or it lives in a fresh capsule. The array must remain portable to be
      shared between domains, so any elements we store in it must also be portable. *)

  (** [set t i a] stores the element [a] at index [i]. Raises [Invalid_arg] if [i] is not
      in the range \[0..length t). *)
  val set : ('a : value mod contended). 'a t @ portable -> int -> 'a @ portable -> unit

  (** [unsafe_set t i a] unsafely stores the element [a] at index [i]. *)
  val unsafe_set
    : ('a : value mod contended).
    'a t @ portable -> int -> 'a @ portable -> unit

  (** [set' t i f] stores [f ()] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val set' : 'a t @ portable -> int -> (unit -> 'a @ portable) @ local portable -> unit

  (** [unsafe_set' t i f] unsafely stores [f ()] at index [i]. *)
  val unsafe_set'
    :  'a t @ portable
    -> int
    -> (unit -> 'a @ portable) @ local portable
    -> unit
end

module type Init = sig @@ portable
  type 'a t
  type 'a init

  (** [init ?grain parallel n ~f] initializes an array with the result of [f] applied to
      the integers 0..n-1. Each block of [grain] indices will be computed sequentially. *)
  val init
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a init
    -> f:(int -> 'a @ portable) @ portable unyielding
    -> 'a t @ portable

  (** See [init]. Allows nested parallelism. *)
  val init'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a init
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ portable) @ portable unyielding
    -> 'a t @ portable
end

module type Reduce = sig @@ portable
  type 'a t

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [iter ?grain parallel t ~f] applies [f] to each element of [t]. Each block of
      [grain] values will be iterated sequentially. *)
  val iter
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:('a @ m portable -> unit) @ portable unyielding
    -> unit

  (** See [iter]. Allows nested parallelism. *)
  val iter'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> 'a @ m portable -> unit) @ portable unyielding
    -> unit

  (** [iteri ?grain parallel t ~f] applies [f] to each element of [t] and its index. Each
      block of [grain] values will be iterated sequentially. *)
  val iteri
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(int -> 'a @ m portable -> unit) @ portable unyielding
    -> unit

  (** See [iteri]. Allows nested parallelism. *)
  val iteri'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m portable -> unit)
       @ portable unyielding
    -> unit

  (** [fold ?grain parallel t ~init ~f ~combine] folds [combine] over the result of
      [map parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. Each block of [grain] values will be folded sequentially. *)
  val fold
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> init:'acc @ contended portable
    -> f:('acc @ contended portable -> 'a @ m portable -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         ('acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable

  (** See [fold]. Allows nested parallelism. *)
  val fold'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> init:'acc @ contended portable
    -> f:
         (Parallel_kernel.t @ local
          -> 'acc @ contended portable
          -> 'a @ m portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         (Parallel_kernel.t @ local
          -> 'acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable

  (** [foldi ?grain parallel t ~init ~f ~combine] folds [combine] over the result of
      [mapi parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. Each block of [grain] values will be folded sequentially. *)
  val foldi
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> init:'acc @ contended portable
    -> f:
         (int
          -> 'acc @ contended portable
          -> 'a @ m portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         ('acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable

  (** See [foldi]. Allows nested parallelism. *)
  val foldi'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> init:'acc @ contended portable
    -> f:
         (Parallel_kernel.t @ local
          -> int
          -> 'acc @ contended portable
          -> 'a @ m portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         (Parallel_kernel.t @ local
          -> 'acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable]

  (** [reduce ?grain parallel t ~f] folds [f] over the elements of [t]. [f] must be
      associative. If [t] is empty, [reduce] returns [None]. Each block of [grain] values
      will be reduced sequentially. *)
  val reduce
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable shared
    -> f:('a @ contended portable -> 'a @ contended portable -> 'a @ contended portable)
       @ portable unyielding
    -> 'a option @ contended portable

  (** See [reduce]. Allows nested parallelism. *)
  val reduce'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable shared
    -> f:
         (Parallel_kernel.t @ local
          -> 'a @ contended portable
          -> 'a @ contended portable
          -> 'a @ contended portable)
       @ portable unyielding
    -> 'a option @ contended portable

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [find ?grain parallel t ~f] returns the first element of [t] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [t]. Each block
      of [grain] values will be tested sequentially. *)
  val find
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:('a @ m portable -> bool) @ portable unyielding
    -> 'a option @ contended portable

  (** See [find]. Allows nested parallelism. *)
  val find'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> 'a @ m portable -> bool) @ portable unyielding
    -> 'a option @ contended portable

  (** [findi ?grain parallel t ~f] returns the first element of [t] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [t] and its
      index. Each block of [grain] values will be tested sequentially. *)
  val findi
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(int -> 'a @ m portable -> bool) @ portable unyielding
    -> 'a option @ contended portable

  (** See [findi]. Allows nested parallelism. *)
  val findi'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m portable -> bool)
       @ portable unyielding
    -> 'a option @ contended portable]
end

module type%template Map = sig @@ portable
  type 'a t

  (** Mapping functions do not need to be templated over the mode of their output type. To
      work with a contended or shared ['b], return a ['b Modes.Contended.t] or
      ['b Modes.Shared.t]. *)

  [@@@mode.default m = (uncontended, shared)]

  (** [map ?grain parallel t ~f] initializes an array with the result of [f] applied to
      each element of [t]. Each block of [grain] values will be computed sequentially. *)
  val map
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:('a @ m portable -> 'b @ portable) @ portable unyielding
    -> 'b t @ portable

  (** See [map]. Allows nested parallelism. *)
  val map'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> 'a @ m portable -> 'b @ portable)
       @ portable unyielding
    -> 'b t @ portable

  (** [mapi ?grain parallel t ~f] initializes an array with the result of [f] applied to
      each element of [t] and its index. Each block of [grain] values will be computed
      sequentially. *)
  val mapi
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(int -> 'a @ m portable -> 'b @ portable) @ portable unyielding
    -> 'b t @ portable

  (** See [mapi]. Allows nested parallelism. *)
  val mapi'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m portable -> 'b @ portable)
       @ portable unyielding
    -> 'b t @ portable

  [@@@mode a = m]
  [@@@mode.default b = (uncontended, shared)]

  (** [map2_exn ?grain parallel t0 t1 ~f] initializes an array with the result of [f]
      applied to each pair of elements of [t0, t1]. Raises if [t0] and [t1] do not have
      equal lengths. Each block of [grain] values will be computed sequentially. *)
  val map2_exn
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a portable
    -> 'b t @ b portable
    -> f:('a @ a portable -> 'b @ b portable -> 'c @ portable) @ portable unyielding
    -> 'c t @ portable

  (** See [map2_exn]. Allows nested parallelism. *)
  val map2_exn'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a portable
    -> 'b t @ b portable
    -> f:
         (Parallel_kernel.t @ local
          -> 'a @ a portable
          -> 'b @ b portable
          -> 'c @ portable)
       @ portable unyielding
    -> 'c t @ portable

  (** [mapi2_exn ?grain parallel t0 t1 ~f] initializes an array with the result of [f]
      applied to each pair of element of [t0, t1] and their index. Raises if [t0] and [t1]
      do not have equal lengths. Each block of [grain] values will be computed
      sequentially. *)
  val mapi2_exn
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a portable
    -> 'b t @ b portable
    -> f:(int -> 'a @ a portable -> 'b @ b portable -> 'c @ portable)
       @ portable unyielding
    -> 'c t @ portable

  (** See [mapi2_exn]. Allows nested parallelism. *)
  val mapi2_exn'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a portable
    -> 'b t @ b portable
    -> f:
         (Parallel_kernel.t @ local
          -> int
          -> 'a @ a portable
          -> 'b @ b portable
          -> 'c @ portable)
       @ portable unyielding
    -> 'c t @ portable
end

module type%template Sort = sig @@ portable
  type 'a t

  [@@@mode.default m = (uncontended, shared)]

  (** [sort ?grain parallel t ~compare] initializes an array with the contents of [t]
      unstably sorted with respect to [compare]. Each block of [grain] values will be
      sorted sequentially. *)
  val sort
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> compare:('a @ local m portable -> 'a @ local m portable -> int)
       @ portable unyielding
    -> 'a t @ m portable

  (** See [sort]. Allows nested parallelism. *)
  val sort'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> compare:
         (Parallel_kernel.t @ local
          -> 'a @ local m portable
          -> 'a @ local m portable
          -> int)
       @ portable unyielding
    -> 'a t @ m portable

  (** [stable_sort ?grain parallel t ~compare] initializes an array with the contents of
      [t] stably sorted with respect to [compare]. Each block of [grain] values will be
      sorted sequentially. *)
  val stable_sort
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> compare:('a @ local m portable -> 'a @ local m portable -> int)
       @ portable unyielding
    -> 'a t @ m portable

  (** See [stable_sort]. Allows nested parallelism. *)
  val stable_sort'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m portable
    -> compare:
         (Parallel_kernel.t @ local
          -> 'a @ local m portable
          -> 'a @ local m portable
          -> int)
       @ portable unyielding
    -> 'a t @ m portable
end

module type Inplace = sig @@ portable
  type 'a t

  (** [map_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each of its elements. Each block of [grain] values will be computed
      sequentially. *)
  val map_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:('a @ portable -> 'a @ portable) @ portable unyielding
    -> unit

  (** See [map_inplace]. Allows nested parallelism. *)
  val map_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:(Parallel_kernel.t @ local -> 'a @ portable -> 'a @ portable)
       @ portable unyielding
    -> unit

  (** [mapi_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each of its elements and their indices. Each block of [grain] values will
      be computed sequentially. *)
  val mapi_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:(int -> 'a @ portable -> 'a @ portable) @ portable unyielding
    -> unit

  (** See [mapi_inplace]. Allows nested parallelism. *)
  val mapi_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ portable -> 'a @ portable)
       @ portable unyielding
    -> unit

  (** [init_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each array index. Each block of [grain] values will be computed
      sequentially. This can be much faster than using [mapi_inplace] since it does not
      need to read the array. *)
  val init_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:(int -> 'a @ portable) @ portable unyielding
    -> unit

  (** See [init_inplace]. Allows nested parallelism. *)
  val init_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ portable) @ portable unyielding
    -> unit

  (** [sort_inplace ?grain parallel t ~compare] unstably sorts [t] with respect to
      [compare]. Each block of [grain] values will be sorted sequentially. *)
  val sort_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> compare:('a @ local -> 'a @ local -> int) @ portable unyielding
    -> unit

  (** See [sort_inplace]. Allows nested parallelism. *)
  val sort_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> compare:(Parallel_kernel.t @ local -> 'a @ local -> 'a @ local -> int)
       @ portable unyielding
    -> unit

  (** [stable_sort_inplace ?grain parallel t ~compare] stably sorts [t] with respect to
      [compare]. Each block of [grain] values will be sorted sequentially. *)
  val stable_sort_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> compare:('a @ local -> 'a @ local -> int) @ portable unyielding
    -> unit

  (** See [stable_sort_inplace]. Allows nested parallelism. *)
  val stable_sort_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ portable
    -> compare:(Parallel_kernel.t @ local -> 'a @ local -> 'a @ local -> int)
       @ portable unyielding
    -> unit
end

module type%template Slice = sig @@ portable
  type 'a array : k with 'a portable

  (** Slices represent a contiguous portion of an array. *)
  type 'a t : k with 'a portable

  (** [length t] returns the number of elements in [t]. *)
  val length : 'a t @ contended local -> int

  [@@@mode.default m = (uncontended, shared)]

  (** [slice ~i ~j array] is a slice representing [array\[i..j)]. *)
  val slice : ?i:int -> ?j:int -> 'a array @ m portable -> 'a t @ local m portable

  (** [sub ~i ~j slice] is a slice representing [slice\[i..j)]. *)
  val sub : ?i:int -> ?j:int -> 'a t @ local m portable -> 'a t @ local m portable

  (** [get t i] reads the element at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val get : 'a t @ local m portable -> int -> 'a @ m portable

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get : 'a t @ local m portable -> int -> 'a @ m portable

  (** [fork_join2 parallel ~pivot t f g] splits the slice [t] into two sub-slices
      representing [t\[0..pivot)] and [t\[pivot..length t)], respectively. The sub-slices
      are passed to [f] and [g], which run in parallel (refer to
      [{Parallel_kernel.fork_join2}]). *)
  val fork_join2
    :  Parallel_kernel.t @ local
    -> ?pivot:int
    -> 'a t @ local m portable
    -> (Parallel_kernel.t @ local -> 'a t @ local m portable -> 'b)
       @ once portable unyielding
    -> (Parallel_kernel.t @ local -> 'a t @ local m portable -> 'c)
       @ once portable unyielding
    -> 'b * 'c
end
[@@kind k = (value mod portable, value mod contended portable)]

module type%template Islice = Slice [@kind value mod contended portable]

module type%template Slice = sig @@ portable
  include Slice [@kind value mod portable] (** @inline *)

  (** To store a value in a parallel array, we must prove that it does not share
      unsynchronized state with any other elements. This is the case if its type crosses
      contention or it lives in a fresh capsule. *)

  (** [set t i a] stores the element [a] at index [i]. Raises [Invalid_arg] if [i] is not
      in the range \[0..length t). *)
  val set
    : ('a : value mod contended).
    'a t @ local portable -> int -> 'a @ portable -> unit

  (** [unsafe_set' t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_set
    : ('a : value mod contended).
    'a t @ local portable -> int -> 'a @ portable -> unit

  (** [set' t i f] stores [f a] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val set'
    :  'a t @ local portable
    -> int
    -> (unit -> 'a @ portable) @ local portable
    -> unit

  (** [unsafe_set' t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_set'
    :  'a t @ local portable
    -> int
    -> (unit -> 'a @ portable) @ local portable
    -> unit
end

module type Parallel_arrays = sig @@ portable
  module type Get = Get
  module type Set = Set
  module type Init = Init
  module type Map = Map
  module type Reduce = Reduce
  module type Sort = Sort
  module type Inplace = Inplace
  module type Islice = Islice
  module type Slice = Slice

  (** The following "parallel array" types are distinct from normal arrays because their
      elements must always be portable and must never share unsynchronized state.

      This is the case when the element type ['a] crosses portability and contention, or
      when the array elements are portable and live in separate capsules (that is, were
      returned at portable uncontended from portable functions).

      To work with a pre-existing array whose element type does not cross portability and
      contention, the elements may be wrapped in [Modes.Portended.t] or [Capsule.Data.t]
      as applicable. *)

  module Array : sig
    type 'a t : mutable_data with 'a portable

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_array : ('a : value mod contended portable). 'a array @ m -> 'a t @ m
    val to_array : ('a : value mod contended portable). 'a t @ m -> 'a array @ m]

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end

  module Iarray : sig
    type 'a t : immutable_data with 'a portable

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_iarray : ('a : value mod contended portable). 'a iarray @ m -> 'a t @ m
    val to_iarray : ('a : value mod contended portable). 'a t @ m -> 'a iarray @ m]

    include Get with type 'a t := 'a t (** @inline *)

    module Slice : Islice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)
  end

  module Vec : sig
    type 'a t : mutable_data with 'a portable

    (** [of_vec] and [to_vec] are the identity operation under the hood. This is memory
        safe because no other domain can get a [Vec.t @ uncontended], so it cannot be
        resized in the middle of a parallel operation. It would be unsafe for [Vec] to
        implement concurrent incremental resizing in general, without updating existing
        code that manipulates it at [uncontended]. It would be similarly unsafe for any
        user of [Vec] to even [Obj.magic_uncontended] it and then resize it. Thus we are
        not introducing any additional memory unsafety in this library. *)

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_vec : ('a : value mod contended portable). 'a Vec.t @ m -> 'a t @ m
    val to_vec : ('a : value mod contended portable). 'a t @ m -> 'a Vec.t @ m]

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end

  module Bigstring : sig
    module Kind = Bigstring.Kind

    type 'a t = 'a Bigstring.t = private
      { kind : 'a Kind.t
      ; data : Base_bigstring.t
      }
    [@@deriving sexp_of]

    val with_kind_exn : 'a Bigstring.Kind.t -> Base_bigstring.t -> 'a t

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init := 'a Bigstring.Kind.t * int

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end
end
