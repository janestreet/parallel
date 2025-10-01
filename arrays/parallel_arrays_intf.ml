open! Base
open! Import

module type Get = sig @@ portable
  type ('a : value mod portable unyielding) t

  (** To read a value from a parallel array, we must prove that it does not escape its
      capsule. This is the case if its type crosses contention, or if it is manipulated
      within a portable function. *)

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [get t i] reads the element at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val get : ('a : value mod contended portable unyielding). 'a t @ m -> int -> 'a @ m

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get
    : ('a : value mod contended portable unyielding).
    'a t @ m -> int -> 'a @ m

  (** [get' t i f] applies [f] with the element read from index [i]. Raises [Invalid_arg]
      if [i] is not in the range \[0..length t). *)
  val get'
    :  'a t @ m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable

  (** [unsafe_get' t i f] applies [f] with the element unsafely read from index [i]. *)
  val unsafe_get'
    :  'a t @ m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable]
end

module type Set = sig @@ portable
  type ('a : value mod portable unyielding) t

  (** To store a value in a parallel array, we must prove that it does not share
      unsynchronized state with any other elements. This is the case if its type crosses
      contention or it lives in a fresh capsule. *)

  (** [set t i a] stores the element [a] at index [i]. Raises [Invalid_arg] if [i] is not
      in the range \[0..length t). *)
  val set : ('a : value mod contended portable unyielding). 'a t -> int -> 'a -> unit

  (** [unsafe_set t i a] unsafely stores the element [a] at index [i]. *)
  val unsafe_set
    : ('a : value mod contended portable unyielding).
    'a t -> int -> 'a -> unit

  (** [set' t i f] stores [f ()] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val set' : 'a t -> int -> (unit -> 'a) @ local once portable -> unit

  (** [unsafe_set' t i f] unsafely stores [f ()] at index [i]. *)
  val unsafe_set' : 'a t -> int -> (unit -> 'a) @ local once portable -> unit
end

module type Init = sig @@ portable
  type ('a : value mod portable unyielding) t
  type 'a init

  (** [init ?grain parallel n ~f] initializes an array with the result of [f] applied to
      the integers 0..n-1. Each block of [grain] indices will be computed sequentially. *)
  val init
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a init
    -> f:(int -> 'a) @ portable unyielding
    -> 'a t

  (** See [init]. Allows nested parallelism. *)
  val init'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a init
    -> f:(Parallel_kernel.t @ local -> int -> 'a) @ portable unyielding
    -> 'a t
end

module type Reduce = sig @@ portable
  type ('a : value mod portable unyielding) t

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [iter ?grain parallel t ~f] applies [f] to each element of [t]. Each block of
      [grain] values will be iterated sequentially. *)
  val iter
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:('a @ m -> unit) @ portable unyielding
    -> unit

  (** See [iter]. Allows nested parallelism. *)
  val iter'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> unit) @ portable unyielding
    -> unit

  (** [iteri ?grain parallel t ~f] applies [f] to each element of [t] and its index. Each
      block of [grain] values will be iterated sequentially. *)
  val iteri
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(int -> 'a @ m -> unit) @ portable unyielding
    -> unit

  (** See [iteri]. Allows nested parallelism. *)
  val iteri'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> unit) @ portable unyielding
    -> unit

  (** [fold ?grain parallel t ~init ~f ~combine] folds [combine] over the result of
      [map parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. Each block of [grain] values will be folded sequentially. *)
  val fold
    : ('acc : value mod portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:(unit -> 'acc) @ portable unyielding
    -> f:('acc -> 'a @ m -> 'acc) @ portable unyielding
    -> combine:('acc -> 'acc -> 'acc) @ portable unyielding
    -> 'acc

  (** See [fold]. Allows nested parallelism. *)
  val fold'
    : ('acc : value mod portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:(unit -> 'acc) @ portable unyielding
    -> f:(Parallel_kernel.t @ local -> 'acc -> 'a @ m -> 'acc) @ portable unyielding
    -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ portable unyielding
    -> 'acc

  (** [foldi ?grain parallel t ~init ~f ~combine] folds [combine] over the result of
      [mapi parallel t ~f]. [combine] must be associative and [combine init x] must equal
      [x]. Each block of [grain] values will be folded sequentially. *)
  val foldi
    : ('acc : value mod portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:(unit -> 'acc) @ portable unyielding
    -> f:(int -> 'acc -> 'a @ m -> 'acc) @ portable unyielding
    -> combine:('acc -> 'acc -> 'acc) @ portable unyielding
    -> 'acc

  (** See [foldi]. Allows nested parallelism. *)
  val foldi'
    : ('acc : value mod portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:(unit -> 'acc) @ portable unyielding
    -> f:(Parallel_kernel.t @ local -> int -> 'acc -> 'a @ m -> 'acc)
       @ portable unyielding
    -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ portable unyielding
    -> 'acc]

  (** [reduce ?grain parallel t ~f] folds [f] over the elements of [t]. [f] must be
      associative. If [t] is empty, [reduce] returns [None]. Each block of [grain] values
      will be reduced sequentially. *)
  val reduce
    : ('a : value mod contended portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ shared
    -> f:('a -> 'a -> 'a) @ portable unyielding
    -> 'a option

  (** See [reduce]. Allows nested parallelism. *)
  val reduce'
    : ('a : value mod contended portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ shared
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a -> 'a) @ portable unyielding
    -> 'a option

  [%%template:
  [@@@mode.default m = (uncontended, shared)]

  (** [find ?grain parallel t ~f] returns the first element of [t] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [t]. Each block
      of [grain] values will be tested sequentially. *)
  val find
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:('a @ m -> bool) @ portable unyielding
    -> 'a option @ m

  (** See [find]. Allows nested parallelism. *)
  val find'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> bool) @ portable unyielding
    -> 'a option @ m

  (** [findi ?grain parallel t ~f] returns the first element of [t] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [t] and its
      index. Each block of [grain] values will be tested sequentially. *)
  val findi
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(int -> 'a @ m -> bool) @ portable unyielding
    -> 'a option @ m

  (** See [findi]. Allows nested parallelism. *)
  val findi'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> bool) @ portable unyielding
    -> 'a option @ m]
end

module type%template Map = sig @@ portable
  type ('a : value mod portable unyielding) t

  (** Mapping functions do not need to be templated over the mode of their output type. To
      work with a contended or shared ['b], return a ['b Modes.Contended.t] or
      ['b Modes.Shared.t]. *)

  [@@@mode.default m = (uncontended, shared)]

  (** [map ?grain parallel t ~f] initializes an array with the result of [f] applied to
      each element of [t]. Each block of [grain] values will be computed sequentially. *)
  val map
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:('a @ m -> 'b) @ portable unyielding
    -> 'b t

  (** See [map]. Allows nested parallelism. *)
  val map'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'b) @ portable unyielding
    -> 'b t

  (** [mapi ?grain parallel t ~f] initializes an array with the result of [f] applied to
      each element of [t] and its index. Each block of [grain] values will be computed
      sequentially. *)
  val mapi
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(int -> 'a @ m -> 'b) @ portable unyielding
    -> 'b t

  (** See [mapi]. Allows nested parallelism. *)
  val mapi'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> 'b) @ portable unyielding
    -> 'b t

  [@@@mode a = m]
  [@@@mode.default b = (uncontended, shared)]

  (** [map2_exn ?grain parallel t0 t1 ~f] initializes an array with the result of [f]
      applied to each pair of elements of [t0, t1]. Raises if [t0] and [t1] do not have
      equal lengths. Each block of [grain] values will be computed sequentially. *)
  val map2_exn
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:('a @ a -> 'b @ b -> 'c) @ portable unyielding
    -> 'c t

  (** See [map2_exn]. Allows nested parallelism. *)
  val map2_exn'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:(Parallel_kernel.t @ local -> 'a @ a -> 'b @ b -> 'c) @ portable unyielding
    -> 'c t

  (** [mapi2_exn ?grain parallel t0 t1 ~f] initializes an array with the result of [f]
      applied to each pair of element of [t0, t1] and their index. Raises if [t0] and [t1]
      do not have equal lengths. Each block of [grain] values will be computed
      sequentially. *)
  val mapi2_exn
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:(int -> 'a @ a -> 'b @ b -> 'c) @ portable unyielding
    -> 'c t

  (** See [mapi2_exn]. Allows nested parallelism. *)
  val mapi2_exn'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ a
    -> 'b t @ b
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ a -> 'b @ b -> 'c)
       @ portable unyielding
    -> 'c t
end

module type%template Sort = sig @@ portable
  type ('a : value mod portable unyielding) t

  [@@@mode.default m = (uncontended, shared)]

  (** [sort ?grain parallel t ~compare] initializes an array with the contents of [t]
      unstably sorted with respect to [compare]. Each block of [grain] values will be
      sorted sequentially. *)
  val sort
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:('a @ local m -> 'a @ local m -> int) @ portable unyielding
    -> 'a t @ m

  (** See [sort]. Allows nested parallelism. *)
  val sort'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:(Parallel_kernel.t @ local -> 'a @ local m -> 'a @ local m -> int)
       @ portable unyielding
    -> 'a t @ m

  (** [stable_sort ?grain parallel t ~compare] initializes an array with the contents of
      [t] stably sorted with respect to [compare]. Each block of [grain] values will be
      sorted sequentially. *)
  val stable_sort
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:('a @ local m -> 'a @ local m -> int) @ portable unyielding
    -> 'a t @ m

  (** See [stable_sort]. Allows nested parallelism. *)
  val stable_sort'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> compare:(Parallel_kernel.t @ local -> 'a @ local m -> 'a @ local m -> int)
       @ portable unyielding
    -> 'a t @ m
end

module type%template Scan = sig @@ portable
  type ('a : value mod portable unyielding) t

  [@@@mode.default m = (uncontended, shared)]

  (** [scan ?grain parallel t ~init ~f] initialises an array containing the exclusive
      prefix sums of [t] with respect to [f]. The first element is [init] and the full
      reduction of [t] is returned separately. [f] must be associative and [f init x] must
      equal [x]. Each block of [grain] values will be computed sequentially. *)
  val scan
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:('a @ m -> 'a @ m -> 'a @ m) @ portable unyielding
    -> 'a t * 'a @ m

  (** See [scan]. Allows nested parallelism. *)
  val scan'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'a @ m -> 'a @ m) @ portable unyielding
    -> 'a t * 'a @ m

  (** [scan_inclusive ?grain parallel t ~init ~f] initialises an array containing the
      inclusive prefix sums of [t] with respect to [f]. The first element is the first
      element of [t]. [f] must be associative and [f init x] must equal [x]. Each block of
      [grain] values will be computed sequentially. *)
  val scan_inclusive
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:('a @ m -> 'a @ m -> 'a @ m) @ portable unyielding
    -> 'a t @ m

  (** See [scan_inclusive]. Allows nested parallelism. *)
  val scan_inclusive'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> init:'a @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'a @ m -> 'a @ m) @ portable unyielding
    -> 'a t @ m
end

module type%template Filter = sig @@ portable
  type ('a : value mod portable unyielding) t

  [@@@mode.default m = (uncontended, shared)]

  (** [filter ?grain parallel t ~f] initialises an array containing the elements of [t]
      that satisfy the predicate [f]. Each block of [grain] values will be computed
      sequentially. *)
  val filter
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:('a @ m -> bool) @ portable unyielding
    -> 'a t @ m

  (** See [filter]. Allows nested parallelism. *)
  val filter'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> bool) @ portable unyielding
    -> 'a t @ m

  (** [filteri ?grain parallel t ~f] initialises an array containing the elements of [t]
      that, alongside their index, satisfy the predicate [f]. Each block of [grain] values
      will be computed sequentially. *)
  val filteri
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(int -> 'a @ m -> bool) @ portable unyielding
    -> 'a t @ m

  (** See [filteri]. Allows nested parallelism. *)
  val filteri'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> bool) @ portable unyielding
    -> 'a t @ m
end

module type%template Filter_map = sig @@ portable
  type ('a : value mod portable unyielding) t

  [@@@mode.default m = (uncontended, shared)]

  (** [filter_map ?grain parallel t ~f] initializes an array with the results of [f]
      applied to each element of [t], filtering out [Null]s. Each block of [grain] values
      will be computed sequentially. *)
  val filter_map
    : ('b : value mod non_float portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:('a @ m -> 'b or_null) @ portable unyielding
    -> 'b t

  (** See [filter_map]. Allows nested parallelism. *)
  val filter_map'
    : ('b : value mod non_float portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> 'a @ m -> 'b or_null) @ portable unyielding
    -> 'b t

  (** [filter_mapi ?grain parallel t ~f] initializes an array with the result of [f]
      applied to each element of [t] and its index, filtering out [Null]s. Each block of
      [grain] values will be computed sequentially. *)
  val filter_mapi
    : ('b : value mod non_float portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(int -> 'a @ m -> 'b or_null) @ portable unyielding
    -> 'b t

  (** See [filter_mapi]. Allows nested parallelism. *)
  val filter_mapi'
    : ('b : value mod non_float portable unyielding).
    ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t @ m
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> 'b or_null) @ portable unyielding
    -> 'b t
end

module type Inplace = sig @@ portable
  type ('a : value mod portable unyielding) t

  (** [map_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each of its elements. Each block of [grain] values will be computed
      sequentially. *)
  val map_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:('a -> 'a) @ portable unyielding
    -> unit

  (** See [map_inplace]. Allows nested parallelism. *)
  val map_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a) @ portable unyielding
    -> unit

  (** [mapi_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each of its elements and their indices. Each block of [grain] values will
      be computed sequentially. *)
  val mapi_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:(int -> 'a -> 'a) @ portable unyielding
    -> unit

  (** See [mapi_inplace]. Allows nested parallelism. *)
  val mapi_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> int -> 'a -> 'a) @ portable unyielding
    -> unit

  (** [init_inplace ?grain parallel t ~f] overwrites an array with the result of [f]
      applied to each array index. Each block of [grain] values will be computed
      sequentially. This can be much faster than using [mapi_inplace] since it does not
      need to read the array. *)
  val init_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:(int -> 'a) @ portable unyielding
    -> unit

  (** See [init_inplace]. Allows nested parallelism. *)
  val init_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> int -> 'a) @ portable unyielding
    -> unit

  (** [sort_inplace ?grain parallel t ~compare] unstably sorts [t] with respect to
      [compare]. Each block of [grain] values will be sorted sequentially. *)
  val sort_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> compare:('a @ local -> 'a @ local -> int) @ portable unyielding
    -> unit

  (** See [sort_inplace]. Allows nested parallelism. *)
  val sort_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> compare:(Parallel_kernel.t @ local -> 'a @ local -> 'a @ local -> int)
       @ portable unyielding
    -> unit

  (** [stable_sort_inplace ?grain parallel t ~compare] stably sorts [t] with respect to
      [compare]. Each block of [grain] values will be sorted sequentially. *)
  val stable_sort_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> compare:('a @ local -> 'a @ local -> int) @ portable unyielding
    -> unit

  (** See [stable_sort_inplace]. Allows nested parallelism. *)
  val stable_sort_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> compare:(Parallel_kernel.t @ local -> 'a @ local -> 'a @ local -> int)
       @ portable unyielding
    -> unit

  (** [scan_inplace ?grain parallel t ~init ~f] overwrites [t] to contain the its
      exclusive prefix sums with respect to [f]. The first element becomes [init] and the
      full reduction of [t] is returned. [f] must be associative and [f init x] must equal
      [x]. Each block of [grain] values will be computed sequentially. *)
  val scan_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:('a -> 'a -> 'a) @ portable unyielding
    -> 'a

  (** See [scan_inplace]. Allows nested parallelism. *)
  val scan_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a -> 'a) @ portable unyielding
    -> 'a

  (** [scan_inclusive_inplace ?grain parallel t ~init ~f] overwrites [t] to contain its
      inclusive prefix sums with respect to [f]. The first element is unchanged. [f] must
      be associative and [f init x] must equal [x]. Each block of [grain] values will be
      computed sequentially. *)
  val scan_inclusive_inplace
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:('a -> 'a -> 'a) @ portable unyielding
    -> unit

  (** See [scan_inclusive_inplace]. Allows nested parallelism. *)
  val scan_inclusive_inplace'
    :  ?grain:int
    -> Parallel_kernel.t @ local
    -> 'a t
    -> init:'a
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a -> 'a) @ portable unyielding
    -> unit
end

module type%template Slice = sig @@ portable
  type ('a : value mod portable unyielding) array : k with 'a

  (** Slices represent a contiguous portion of an array. *)
  type ('a : value mod portable unyielding) t : k with 'a

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
  val get
    : ('a : value mod contended portable unyielding).
    'a t @ local m -> int -> 'a @ m

  (** [unsafe_get t i] unsafely reads the element at index [i]. *)
  val unsafe_get
    : ('a : value mod contended portable unyielding).
    'a t @ local m -> int -> 'a @ m

  (** [get' t i f] applies [f] with the element read from index [i]. Raises [Invalid_arg]
      if [i] is not in the range \[0..length t). *)
  val get'
    :  'a t @ local m
    -> int
    -> ('a @ m -> 'b @ contended portable) @ local once portable
    -> 'b @ contended portable

  (** [unsafe_get' t i f] applies [f] with the element unsafely read from index [i]. *)
  val unsafe_get'
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
    -> (Parallel_kernel.t @ local -> 'a t @ local m -> 'b) @ once portable unyielding
    -> (Parallel_kernel.t @ local -> 'a t @ local m -> 'c) @ once portable unyielding
    -> #('b * 'c)

  (** [for_ parallel ~pivots t ~f] splits the slice [t] into multiple sub-slices
      representing the ranges [t\[0..pivots[0])], [t\[pivots[i]..pivots[i+1])], etc. The
      function [f] is evaluated for each sub-slice in parallel. [pivots] must be
      non-decreasing, but may have duplicate elements, resulting in empty sub-slices. *)
  val for_
    :  Parallel_kernel.t @ local
    -> pivots:int iarray
    -> 'a t @ local m
    -> f:(Parallel_kernel.t @ local -> 'a t @ local m -> unit) @ portable unyielding
    -> unit

  (** [fori parallel ~pivots t ~f] splits the slice [t] into multiple sub-slices
      representing the ranges [t\[0..pivots[0])], [t\[pivots[i]..pivots[i+1])], etc. The
      function [f] is evaluated for each sub-slice and its index in parallel. [pivots]
      must be non-decreasing, but may have duplicate elements. *)
  val fori
    :  Parallel_kernel.t @ local
    -> pivots:int iarray
    -> 'a t @ local m
    -> f:(Parallel_kernel.t @ local -> int -> 'a t @ local m -> unit)
       @ portable unyielding
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
  val set
    : ('a : value mod contended portable unyielding).
    'a t @ local -> int -> 'a -> unit

  (** [unsafe_set' t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_set
    : ('a : value mod contended portable unyielding).
    'a t @ local -> int -> 'a -> unit

  (** [set' t i f] stores [f a] at index [i]. Raises [Invalid_arg] if [i] is not in the
      range \[0..length t). *)
  val set' : 'a t @ local -> int -> (unit -> 'a) @ local once portable -> unit

  (** [unsafe_set' t i f] unsafely stores [f a] at index [i]. *)
  val unsafe_set' : 'a t @ local -> int -> (unit -> 'a) @ local once portable -> unit
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
    type ('a : value mod portable unyielding) t : mutable_data with 'a

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_array
      : ('a : value mod contended portable unyielding).
      'a array @ m -> 'a t @ m

    val to_array
      : ('a : value mod contended portable unyielding).
      'a t @ m -> 'a array @ m]

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
    type ('a : value mod portable unyielding) t : immutable_data with 'a

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_iarray
      : ('a : value mod contended portable unyielding).
      'a iarray @ m -> 'a t @ m

    val to_iarray
      : ('a : value mod contended portable unyielding).
      'a t @ m -> 'a iarray @ m]

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
  end

  module Vec : sig
    type ('a : value mod portable unyielding) t : mutable_data with 'a

    (** [of_vec] and [to_vec] are the identity operation under the hood. This is memory
        safe because no other domain can get a [Vec.t @ uncontended], so it cannot be
        resized in the middle of a parallel operation. It would be unsafe for [Vec] to
        implement concurrent incremental resizing in general, without updating existing
        code that manipulates it at [uncontended]. It would be similarly unsafe for any
        user of [Vec] to even [Obj.magic_uncontended] it and then resize it. Thus we are
        not introducing any additional memory unsafety in this library. *)

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val of_vec : ('a : value mod contended portable unyielding). 'a Vec.t @ m -> 'a t @ m
    val to_vec : ('a : value mod contended portable unyielding). 'a t @ m -> 'a Vec.t @ m]

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

    (** @inline *)
    include Init with type 'a t := 'a t and type 'a init := 'a Bigstring.Kind.t * int

    include Reduce with type 'a t := 'a t (** @inline *)

    include Sort with type 'a t := 'a t (** @inline *)

    include Scan with type 'a t := 'a t (** @inline *)

    include Filter with type 'a t := 'a t (** @inline *)

    include Inplace with type 'a t := 'a t (** @inline *)
  end
end
