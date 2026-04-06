open! Base
module Parallel = Parallel_kernel

module Definitions = struct
  (** Parallel operations over maps.

      Most of these implement parallel versions of [Map.*i] traversals. That is, we pass
      [~key ~data] to all [~f] arguments. We leave off the [i] suffix, which is redundant
      here. Providing both [i] and non-[i] versions seems like unnecessary bloat to this
      interface. *)
  module type S = sig
    type ('key, 'data, 'cmp) map
    type ('key, 'cmp, 'fn) with_comparator

    (** Parallel version of [Map.fold]. *)
    val fold
      : ('key : value mod contended portable) ('data : value mod contended portable) 'res.
      Parallel.t @ local
      -> ('key, 'data, 'cmp) map
      -> init:(unit -> 'res) @ portable
      -> f:(Parallel.t @ local -> key:'key -> data:'data -> 'res -> 'res) @ shareable
      -> combine:(Parallel.t @ local -> 'res -> 'res -> 'res) @ shareable
      -> 'res

    (** Parallel version of [Map.iteri]. *)
    val iter
      : ('key : value mod contended portable) ('data : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data -> unit) @ shareable
      -> unit

    (** Parallel version of [Map.mapi]. *)
    val map
      : ('key : value mod contended portable) ('data1 : value mod contended portable)
        ('data2 : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data1, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data1 -> 'data2) @ shareable
      -> ('key, 'data2, 'cmp) map

    (** Parallel version of [Map.filteri]. *)
    val filter
      : ('key : value mod contended portable) ('data : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data -> bool) @ shareable
      -> ('key, 'data, 'cmp) map

    (** Parallel version of [Map.filter_mapi]. *)
    val filter_map
      : ('key : value mod contended portable) ('data1 : value mod contended portable)
        ('data2 : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data1, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data1 -> 'data2 option) @ shareable
      -> ('key, 'data2, 'cmp) map

    (** Parallel version of [Map.partitioni_tf]. *)
    val partition_tf
      : ('key : value mod contended portable) ('data : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data -> bool) @ shareable
      -> ('key, 'data, 'cmp) map * ('key, 'data, 'cmp) map

    (** Parallel version of [Map.partition_mapi]. *)
    val partition_map
      : ('key : value mod contended portable) ('data1 : value mod contended portable)
        ('data2 : value mod contended portable) ('data3 : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data1, 'cmp) map
      -> f:(Parallel.t @ local -> key:'key -> data:'data1 -> ('data2, 'data3) Either.t)
         @ shareable
      -> ('key, 'data2, 'cmp) map * ('key, 'data3, 'cmp) map

    (** Parallel version of [Map.merge]. *)
    val merge_filter_map
      : ('key : value mod contended portable) ('data1 : value mod contended portable)
        ('data2 : value mod contended portable) ('data3 : value mod contended portable)
        ('cmp : value mod portable).
      ( 'key
        , 'cmp
        , Parallel.t @ local
          -> ('key, 'data1, 'cmp) map
          -> ('key, 'data2, 'cmp) map
          -> f:
               (Parallel.t @ local
                -> key:'key
                -> ('data1, 'data2) Map.Merge_element.t
                -> 'data3 option)
             @ shareable
          -> ('key, 'data3, 'cmp) map )
        with_comparator

    (** Constructs a parallel sequence from a map. *)
    val to_sequence
      : ('key : value mod contended portable) ('data : value mod contended portable).
      ('key, 'data, 'cmp) map -> ('key * 'data) Parallel_sequence.With_length.t @ local

    (** Constructs a parallel sequence from a pair of maps sharing a key type. *)
    val to_sequence2
      : ('key : value mod contended portable) ('data1 : value mod contended portable)
        ('data2 : value mod contended portable) ('cmp : value mod portable).
      ( 'key
        , 'cmp
        , ('key, 'data1, 'cmp) map
          -> ('key, 'data2, 'cmp) map
          -> ('key * ('data1, 'data2) Map.Merge_element.t) Parallel_sequence.t @ local )
        with_comparator

    (** Parallel computation following the tree structure of a map. *)
    val traverse
      : ('key : value mod contended portable) ('data : value mod contended portable).
      Parallel.t @ local
      -> ('key, 'data, 'cmp) map
      -> on_empty:(unit -> 'res) @ shareable
      -> on_data:(Parallel.t @ local -> key:'key -> data:'data -> 'v) @ shareable
      -> on_leaf:(Parallel.t @ local -> key:'key -> 'v -> 'res) @ shareable
      -> on_node:(Parallel.t @ local -> key:'key -> 'v -> 'res -> 'res -> 'res)
         @ shareable
      -> 'res
  end
end

module type Parallel_map = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Tree : sig
    (** Parallel operations on [_ Map.Tree.t]. *)

    (** @inline *)
    include
      S
      with type ('key, 'data, 'cmp) map := ('key, 'data, 'cmp) Map.Tree.t
       and type ('key, 'cmp, 'fn) with_comparator :=
        comparator:('key, 'cmp) Comparator.t -> 'fn
  end

  (** Parallel operations on [_ Map.t]. *)

  (** @inline *)
  include
    S
    with type ('key, 'data, 'cmp) map := ('key, 'data, 'cmp) Map.t
     and type ('key, 'cmp, 'fn) with_comparator := 'fn
end
