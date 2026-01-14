open! Base
open! Import

module type Get = sig @@ portable
  type ('a : value mod portable) t

  (** To read a value from a parallel array, we must prove that it does not escape its
      capsule. This is the case if its type crosses contention, or if it is manipulated
      within a portable function. *)

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [get t i] reads the element at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val get : ('a : value mod contended portable). 'a t @ m -> int -> 'a @ m

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get : ('a : value mod contended portable). 'a t @ m -> int -> 'a @ m

  (** [extract t i f] applies [f] with the element read from index [i]. Raises
      [Invalid_arg] if [i] is not in the range \[0..length t). *)
  val extract
    :  'a t @ m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable

  (** [unsafe_extract t i f] applies [f] with the element unsafely read from index [i]. *)
  val unsafe_extract
    :  'a t @ m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable]
end

module type Set = sig @@ portable
  type ('a : value mod portable) t

  (** To store a value in a parallel array, we must prove that it does not share
      unsynchronized state with any other elements. This is the case if its type crosses
      contention or it lives in a fresh capsule. *)

  (** [set t i a] stores the element [a] at index [i]. Raises [Invalid_arg] if [i] is not
      in the range \[0..length t). *)
  val set : ('a : value mod contended portable). 'a t -> int -> 'a -> unit

  (** [unsafe_set t i a] unsafely stores the element [a] at index [i]. *)
  val unsafe_set : ('a : value mod contended portable). 'a t -> int -> 'a -> unit

  (** [insert t i f] stores [f ()] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val insert : 'a t -> int -> (unit -> 'a) @ local once portable -> unit

  (** [unsafe_insert t i f] unsafely stores [f ()] at index [i]. *)
  val unsafe_insert : 'a t -> int -> (unit -> 'a) @ local once portable -> unit
end

module type Init = sig @@ portable
  type ('a : value mod portable) t
  type 'a init

  (** [init parallel n ~f] initializes an array with the result of [f] applied to the
      integers 0..n-1. *)
  val init
    :  Parallel_kernel.t @ local
    -> 'a init
    -> f:(Parallel_kernel.t @ local -> int -> 'a) @ shareable
    -> 'a t
end

module type Reduce = sig @@ portable
  type ('a : value mod portable) t

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [iter parallel t ~f] applies [f] to each element of [t]. *)
  val iter
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> unit) @ shareable
    -> unit

  (** [iteri parallel t ~f] applies [f] to each element of [t] and its index. *)
  val iteri
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> unit) @ shareable
    -> unit

  (** [find parallel t ~f] returns the first element of [t] for which [f] returns [true],
      if it exists. [f] will always be applied to every element of [t]. *)
  val find
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> bool) @ shareable
    -> 'a option @ m

  (** [findi parallel t ~f] returns the first element of [t] for which [f] returns [true],
      if it exists. [f] will always be applied to every element of [t] and its index. *)
  val findi
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> bool) @ shareable
    -> 'a option @ m

  (** [reduce parallel t ~f] folds [f] over the elements of [t]. [f] must be associative.
      If [t] is empty, [reduce] returns [None]. *)
  val reduce
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a @ m) @ shareable
    -> 'a option @ m

  (** [min_elt parallel t ~compare] is the minimum element of [t] according to [compare].
      If [t] is empty, returns [None]. *)
  val min_elt
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> 'a option @ m

  (** [max_elt parallel t ~compare] is the maximum element of [t] according to [compare].
      If [t] is empty, returns [None]. *)
  val max_elt
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> 'a option @ m]

  (** [fold parallel t ~init ~f ~combine] folds [combine] over the result of
      [map parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. *)
  val fold
    :  Parallel_kernel.t @ local
    -> 'a t @ shared
    -> init:(unit -> 'acc) @ portable
    -> f:(Parallel_kernel.t @ local -> 'acc -> 'a @ shared -> 'acc) @ shareable
    -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ shareable
    -> 'acc

  (** [foldi parallel t ~init ~f ~combine] folds [combine] over the result of
      [mapi parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. *)
  val foldi
    :  Parallel_kernel.t @ local
    -> 'a t @ shared
    -> init:(unit -> 'acc) @ portable
    -> f:(Parallel_kernel.t @ local -> int -> 'acc -> 'a @ shared -> 'acc) @ shareable
    -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ shareable
    -> 'acc
end

module type%template Map = sig @@ portable
  type ('a : value mod portable) t

  (** Mapping functions do not need to be templated over the mode of their output type. To
      work with a contended or shared ['b], return a ['b Modes.Contended.t] or
      ['b Modes.Shared.t]. *)

  [@@@mode.default m = (uncontended, shared)]

  (** [map parallel t ~f] initializes an array with the result of [f] applied to each
      element of [t]. *)
  val map
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'b) @ shareable
    -> 'b t

  (** [mapi parallel t ~f] initializes an array with the result of [f] applied to each
      element of [t] and its index. *)
  val mapi
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> 'b) @ shareable
    -> 'b t

  [@@@mode a = m]
  [@@@mode.default b = (uncontended, shared)]

  (** [map2_exn parallel t0 t1 ~f] initializes an array with the result of [f] applied to
      each pair of elements of [t0, t1]. Raises if [t0] and [t1] do not have equal
      lengths. *)
  val map2_exn
    :  Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:(Parallel_kernel.t @ local -> 'a @ a -> 'b @ b -> 'c) @ shareable
    -> 'c t

  (** [mapi2_exn parallel t0 t1 ~f] initializes an array with the result of [f] applied to
      each pair of element of [t0, t1] and their index. Raises if [t0] and [t1] do not
      have equal lengths. *)
  val mapi2_exn
    :  Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ a -> 'b @ b -> 'c) @ shareable
    -> 'c t
end

module type%template Sort = sig @@ portable
  type ('a : value mod portable) t

  [@@@mode.default m = (uncontended, shared)]

  (** [sort parallel t ~compare] initializes an array with the contents of [t] unstably
      sorted with respect to [compare]. *)
  val sort
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> 'a t @ m

  (** [stable_sort parallel t ~compare] initializes an array with the contents of [t]
      stably sorted with respect to [compare]. *)
  val stable_sort
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> 'a t @ m
end

module type%template Scan = sig @@ portable
  type ('a : value mod portable) t

  [@@@mode.default m = (uncontended, shared)]

  (** [scan parallel t ~init ~f] initialises an array containing the exclusive prefix sums
      of [t] with respect to [f]. The first element is [init] and the full reduction of
      [t] is returned separately. [f] must be associative and [f init x] must equal [x]. *)
  val scan
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a @ m) @ shareable
    -> 'a t * 'a @ m

  (** [scan_inclusive parallel t ~init ~f] initialises an array containing the inclusive
      prefix sums of [t] with respect to [f]. The first element is the first element of
      [t]. [f] must be associative and [f init x] must equal [x]. *)
  val scan_inclusive
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a @ m) @ shareable
    -> 'a t @ m
end

module type%template Filter = sig @@ portable
  type ('a : value mod portable) t

  [@@@mode.default m = (uncontended, shared)]

  (** [filter parallel t ~f] initialises an array containing the elements of [t] that
      satisfy the predicate [f]. *)
  val filter
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> bool) @ shareable
    -> 'a t @ m

  (** [filteri parallel t ~f] initialises an array containing the elements of [t] that,
      alongside their index, satisfy the predicate [f]. *)
  val filteri
    :  Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> bool) @ shareable
    -> 'a t @ m
end

module type%template Filter_map = sig @@ portable
  type ('a : value mod portable) t

  [@@@mode.default m = (uncontended, shared)]

  (** [filter_map parallel t ~f] initializes an array with the results of [f] applied to
      each element of [t], filtering out [Null]s. *)
  val filter_map
    : ('b : value mod non_float portable).
    Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'b or_null) @ shareable
    -> 'b t

  (** [filter_mapi parallel t ~f] initializes an array with the result of [f] applied to
      each element of [t] and its index, filtering out [Null]s. *)
  val filter_mapi
    : ('b : value mod non_float portable).
    Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> 'b or_null) @ shareable
    -> 'b t
end

module type Inplace = sig @@ portable
  type ('a : value mod portable) t

  (** [map_inplace parallel t ~f] overwrites an array with the result of [f] applied to
      each of its elements. *)
  val map_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a) @ shareable
    -> unit

  (** [mapi_inplace parallel t ~f] overwrites an array with the result of [f] applied to
      each of its elements and their indices. *)
  val mapi_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> int -> 'a -> 'a) @ shareable
    -> unit

  (** [init_inplace parallel t ~f] overwrites an array with the result of [f] applied to
      each array index. This can be much faster than using [mapi_inplace] since it does
      not need to read the array. *)
  val init_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> int -> 'a) @ shareable
    -> unit

  (** [sort_inplace parallel t ~compare] unstably sorts [t] with respect to [compare]. *)
  val sort_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> unit

  (** [stable_sort_inplace parallel t ~compare] stably sorts [t] with respect to
      [compare]. *)
  val stable_sort_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> compare:
         (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
       @ shareable
    -> unit

  (** [scan_inplace parallel t ~init ~f] overwrites [t] to contain the its exclusive
      prefix sums with respect to [f]. The first element becomes [init] and the full
      reduction of [t] is returned. [f] must be associative and [f init x] must equal [x]. *)
  val scan_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a) @ shareable
    -> 'a

  (** [scan_inclusive_inplace parallel t ~init ~f] overwrites [t] to contain its inclusive
      prefix sums with respect to [f]. The first element is unchanged. [f] must be
      associative and [f init x] must equal [x]. *)
  val scan_inclusive_inplace
    :  Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a) @ shareable
    -> unit
end

module type%template Slice = sig @@ portable
  type ('a : value mod portable) array : k with 'a

  (** Slices represent a contiguous portion of an array. *)
  type ('a : value mod portable) t : k with 'a

  (** [length t] returns the number of elements in [t]. *)
  val length : 'a t @ contended local -> int

  [@@@mode.default m = (uncontended, shared)]

  (** [slice ~i ~j array] is a slice representing [array\[i..j)]. *)
  val slice : ?i:int -> ?j:int -> 'a array @ m -> 'a t @ local m

  (** [sub ~i ~j slice] is a slice representing [slice\[i..j)]. *)
  val sub : ?i:int -> ?j:int -> 'a t @ local m -> 'a t @ local m

  (** To read a value from a parallel array, we must prove that it does not escape its
      capsule. This is the case if its type crosses contention, or if it is manipulated
      within a portable function. *)

  (** [get t i] reads the element at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val get : ('a : value mod contended portable). 'a t @ local m -> int -> 'a @ m

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get : ('a : value mod contended portable). 'a t @ local m -> int -> 'a @ m

  (** [extract t i f] applies [f] with the element read from index [i]. Raises
      [Invalid_arg] if [i] is not in the range \[0..length t). *)
  val extract
    :  'a t @ local m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable

  (** [unsafe_extract t i f] applies [f] with the element unsafely read from index [i]. *)
  val unsafe_extract
    :  'a t @ local m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable

  (** [fork_join2 parallel ~pivot t f g] splits the slice [t] into two sub-slices
      representing [t\[0..pivot)] and [t\[pivot..length t)], respectively. The sub-slices
      are passed to [f] and [g], which run in parallel (refer to
      [{Parallel_kernel.fork_join2}]). *)
  val fork_join2
    :  Parallel_kernel.t @ local
    -> ?pivot:int
    -> 'a t @ local m
    -> (Parallel_kernel.t @ local -> 'a t @ local m -> 'b) @ forkable local once shareable
    -> (Parallel_kernel.t @ local -> 'a t @ local m -> 'c) @ once shareable
    -> #('b * 'c)

  (** [for_ parallel ~pivots t ~f] splits the slice [t] into multiple sub-slices
      representing the ranges [t\[0..pivots[0])], [t\[pivots[i]..pivots[i+1])], etc. The
      function [f] is evaluated for each sub-slice in parallel. [pivots] must be
      non-decreasing, but may have duplicate elements, resulting in empty sub-slices. *)
  val for_
    :  Parallel_kernel.t @ local
    -> pivots:int iarray
    -> 'a t @ local m
    -> f:(Parallel_kernel.t @ local -> 'a t @ local m -> unit) @ shareable
    -> unit

  (** [fori parallel ~pivots t ~f] splits the slice [t] into multiple sub-slices
      representing the ranges [t\[0..pivots[0])], [t\[pivots[i]..pivots[i+1])], etc. The
      function [f] is evaluated for each sub-slice and its index in parallel. [pivots]
      must be non-decreasing, but may have duplicate elements. *)
  val fori
    :  Parallel_kernel.t @ local
    -> pivots:int iarray
    -> 'a t @ local m
    -> f:(Parallel_kernel.t @ local -> int -> 'a t @ local m -> unit) @ shareable
    -> unit
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
  val set : ('a : value mod contended portable). 'a t @ local -> int -> 'a -> unit

  (** [unsafe_insert t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_set : ('a : value mod contended portable). 'a t @ local -> int -> 'a -> unit

  (** [insert t i f] stores [f a] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val insert : 'a t @ local -> int -> (unit -> 'a) @ local once portable -> unit

  (** [unsafe_insert t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_insert : 'a t @ local -> int -> (unit -> 'a) @ local once portable -> unit
end

module type Parallel_arrays = sig @@ portable
  module type Get = Get
  module type Set = Set
  module type Init = Init
  module type Map = Map
  module type Reduce = Reduce
  module type Sort = Sort
  module type Scan = Scan
  module type Filter = Filter
  module type Filter_map = Filter_map
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
    type ('a : value mod portable) t : mutable_data with 'a

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_array : ('a : value mod contended portable). 'a array @ m -> 'a t @ m
    val to_array : ('a : value mod contended portable). 'a t @ m -> 'a array @ m]

    (** [length t] returns the number of elements in [t]. *)
    val length : 'a t @ contended -> int

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Filter_map with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end

  module Iarray : sig
    type ('a : value mod portable) t : immutable_data with 'a

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_iarray : ('a : value mod contended portable). 'a iarray @ m -> 'a t @ m
    val to_iarray : ('a : value mod contended portable). 'a t @ m -> 'a iarray @ m]

    (** [length t] returns the number of elements in [t]. *)
    val length : 'a t @ contended -> int

    include Get with type 'a t := 'a t (** @inline *)

    module Slice : Islice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Filter_map with type 'a t := 'a t (** @inline *)
  end

  module Vec : sig
    type ('a : value mod portable) t : mutable_data with 'a

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

    (** [length t] returns the number of elements in [t]. *)
    val length : 'a t @ shared -> int

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init = int

    include Map with type 'a t := 'a t (** @inline *)

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end

  module Bigstring : sig
    module Kind = Bigstring.Kind

    type 'a t = 'a Bigstring.t = private
      { kind : 'a Kind.t
      ; data : Base_bigstring.t
      }
    [@@deriving sexp_of]

    val%template with_kind_exn : 'a Bigstring.Kind.t -> Base_bigstring.t @ m -> 'a t @ m
    [@@mode m = (uncontended, shared)]

    (** [length t] returns the number of elements in [t]. *)
    val length : 'a t @ contended -> int

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    val%template of_slice : 'a Slice.t @ local m -> 'a t @ m
    [@@mode m = (uncontended, shared)]

    (** [of_string s] is a new character bigstring initialized by copying the contents of
        the string [s]. The optional arguments [pos] and [len] are forwarded to
        {!Base_bigstring.of_string} *)
    val of_string : ?pos:int -> ?len:int -> string -> char t

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init := 'a Kind.t * int

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end

  module Bigarray : sig
    module Kind : sig
      type ('a, 'k) t = ('a, 'k) Bigarray.kind =
        | Float32 : (float, Bigarray.float32_elt) t
        | Float64 : (float, Bigarray.float64_elt) t
        | Int8_signed : (int, Bigarray.int8_signed_elt) t
        | Int8_unsigned : (int, Bigarray.int8_unsigned_elt) t
        | Int16_signed : (int, Bigarray.int16_signed_elt) t
        | Int16_unsigned : (int, Bigarray.int16_unsigned_elt) t
        | Int32 : (int32, Bigarray.int32_elt) t
        | Int64 : (int64, Bigarray.int64_elt) t
        | Int : (int, Bigarray.int_elt) t
        | Nativeint : (nativeint, Bigarray.nativeint_elt) t
        | Complex32 : (Stdlib.Complex.t, Bigarray.complex32_elt) t
        | Complex64 : (Stdlib.Complex.t, Bigarray.complex64_elt) t
        | Char : (char, Bigarray.int8_unsigned_elt) t
        | Float16 : (float, Bigarray.float16_elt) t
    end

    module Layout : sig
      type 'a t = 'a Bigarray.layout =
        | C_layout : Bigarray.c_layout t
        | Fortran_layout : Bigarray.fortran_layout t
    end

    module Spec : sig
      type 'a t = T : ('a, _) Kind.t * _ Layout.t -> 'a t
    end

    type 'a t = T : ('a, _, _) Bigarray.Array1.t -> 'a t [@@unboxed]

    (** Wraps a [Bigarray.Array1.t] as a parallel bigarray. *)
    val%template of_bigarray : ('a, _, _) Bigarray.Array1.t @ m -> 'a t @ m
    [@@mode m = (uncontended, shared)]

    (** Wraps a [Bigarray.kind] and [Bigarray.layout] as a parallel bigarray spec. *)
    val kind : ('a, _) Kind.t -> _ Layout.t -> 'a Spec.t

    (** [length t] returns the number of elements in [t]. *)
    val length : 'a t @ contended -> int

    include Get with type 'a t := 'a t (** @inline *)

    include Set with type 'a t := 'a t (** @inline *)

    module Slice : Slice with type 'a array := 'a t

    val%template of_slice : 'a Slice.t @ local m -> 'a t @ m
    [@@mode m = (uncontended, shared)]

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init := 'a Spec.t * int

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end
end
