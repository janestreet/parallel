open! Base
open! Import

module type S = sig @@ portable
  (** ['a t] represents a finite sequence of independent computations that produce an
      ['a]. These computations are evaluated in parallel upon collecting the sequence into
      a strict data structure. Parallel sequences are distinguished from normal sequences
      by supporting the [split] operation, which divides a sequence into two parts that
      may be collected in parallel. *)
  type ('a : value mod portable) t : value mod contended portable

  (** The empty sequence. *)
  val empty : 'a t

  (** [globalize seq] copies a local sequence to the heap. *)
  val globalize : 'a t @ local -> 'a t

  (** [range ?stride ?start ?stop start_i stop_i] is the sequence of integers from
      [start_i] to [stop_i], stepping by [stride]. If [stride] < 0 then we need [start_i]
      > [stop_i] for the result to be nonempty (or [start_i] >= [stop_i] in the case where
      both bounds are inclusive). *)
  val range
    :  ?stride:int (** default = 1 *)
    -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
    -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
    -> int
    -> int
    -> int t @ local

  (** [init n ~f] is a sequence containing [f _ i] for each i in [0..n-1]. *)
  val init : int -> f:(Parallel_kernel.t @ local -> int -> 'a) @ portable -> 'a t @ local

  (** [of_iarray i] is a sequence containing the contents of the iarray [i]. This sequence
      has a known length and may be split in constant time. *)
  val of_iarray : ('a : value mod contended portable). 'a iarray -> 'a t @ local

  (** [append s0 s1] returns a sequence containing the elements of [s0] and then the
      elements of [s1]. *)
  val append : 'a t @ local -> 'a t @ local -> 'a t @ local

  (** [product_left seq0 seq1] joins an ['a] sequence and a ['b] sequence into an
      [('a * 'b)] sequence by pairing each element of [seq0] with a copy of [seq1]. *)
  val product_left
    : ('a : value mod contended portable).
    'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

  (** [product_right seq0 seq1] joins an ['a] sequence and a ['b] sequence into an
      [('a * 'b)] sequence by pairing each element of [seq1] with a copy of [seq0]. *)
  val product_right
    : ('b : value mod contended portable).
    'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

  (** [map seq ~f] converts an ['a] sequence to a ['b] sequence by applying [f] to each
      element of [seq]. *)
  val map
    :  'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b) @ portable
    -> 'b t @ local

  (** [iter parallel seq ~f] applies [f] to each element of [seq] in parallel. The order
      in which [f] is applied is unspecified and potentially non-deterministic. *)
  val iter
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> unit) @ shareable
    -> unit

  (** [fold parallel seq ~f ~init ~combine] folds [combine] over [map seq ~f] in parallel.
      [combine] and [init] must form a monoid over ['acc]: [combine] must be associative
      and [init ()] must be a neutral element. The order in which [f] and [combine] are
      applied is unspecified and potentially non-deterministic. *)
  val fold
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> init:(unit -> 'acc) @ portable
    -> f:(Parallel_kernel.t @ local -> 'acc -> 'a -> 'acc) @ shareable
    -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ shareable
    -> 'acc

  (** [reduce parallel seq ~f] reduces [seq] using [f] in parallel. [f] must be
      associative. If the sequence is empty, [reduce] returns [None]. The order in which
      [f] is applied is unspecified and potentially non-deterministic. *)
  val reduce
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> 'a -> 'a) @ shareable
    -> 'a option

  (** [find parallel seq ~f] returns the first element of [seq] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [seq]. The
      order in which [f] is applied is unspecified and potentially non-deterministic. *)
  val find
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> bool) @ shareable
    -> 'a option

  (** [to_list seq] collects a sequence into a list by evaluating each element in
      parallel. Requires iterating the sequence sequentially. *)
  val to_list : Parallel_kernel.t @ local -> 'a t @ local -> 'a list

  (** [to_iarray seq] collects a sequence into an iarray by evaluating each element in
      parallel. If the sequence has a known length, does not require iterating the
      sequence sequentially. *)
  val to_iarray : Parallel_kernel.t @ local -> 'a t @ local -> 'a iarray
end

module type Parallel_sequence = sig @@ portable
  module type S = S

  include S (** @inline *)

  (** [unfold ~init ~next ~split] creates a sequence representing the stream of values
      produced by recursively applying [next] to [init].

      [next state] returns either [Some (a,s)], representing the element [a] and the new
      state [s], or [None], representing the end of the sequence.

      [split state] optionally returns two states [s0, s1] such that the concatenation of
      the unfoldings of [s0] and [s1] produces the same elements as the original sequence.
      [split] must return [Some] whenever the sequence contains more than one element. *)
  val unfold
    : ('s : value mod contended portable).
    init:'s
    -> next:
         (Parallel_kernel.t @ local
          -> 's
          -> (#('a * 's) Option_u.t[@kind value_or_null & value_or_null]))
       @ portable
    -> split:
         (Parallel_kernel.t @ local
          -> 's
          -> (#('s * 's) Option_u.t[@kind value_or_null & value_or_null]))
       @ portable
    -> 'a t @ local

  (** [concat seqs] creates a sequence representing the concatenation of all sequences in
      [seqs]. When possible, splitting the resulting sequence is equivalent to splitting
      the top-level input sequence. Otherwise, splitting separates out only the first
      element of [seqs]. *)
  val concat : 'a t t @ local -> 'a t @ local

  (** [concat_map seq ~f] is equivalent to [map seq ~f |> concat]. *)
  val concat_map
    :  'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b t) @ portable
    -> 'b t @ local

  (** [filter_map seq ~f] converts an ['a] sequence to a ['b] sequence containing only the
      elements of [seq] such that [f] returns [Some b]. *)
  val filter_map
    :  'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b option) @ portable
    -> 'b t @ local

  (** Sequences of type ['a With_length.t] are guaranteed to have a known length. When the
      length is known, collecting the sequence can be more efficient. Note that [concat]
      and [filter] functions are not supported. *)
  module With_length : sig
    include S (** @inline *)

    (** [length seq] returns the number of elements in [seq]. *)
    val length : 'a t @ local -> int

    (** [unfold ~init ~next ~split_at ~length] creates a sequence representing the stream
        of values produced by recursively applying [next] to [init].

        [next state] returns either [Some (a,s)], representing the element [a] and the new
        state [s], or [None], representing the end of the sequence.

        [length state] must return the exact number of elements [next state] will produce
        before returning [None].

        [split_at state ~n] optionally returns two states [s0, s1] such that
        [length s0 = n] and the concatenation of the unfoldings of [s0] and [s1] produces
        the same elements as the original sequence. [split_at] must return [Some] whenever
        [n > 0 && n < length state] *)
    val unfold
      : ('s : value mod contended portable).
      init:'s
      -> next:
           (Parallel_kernel.t @ local
            -> 's
            -> (#('a * 's) Option_u.t[@kind value_or_null & value_or_null]))
         @ portable
      -> split_at:
           (Parallel_kernel.t @ local
            -> 's
            -> n:int
            -> (#('s * 's) Option_u.t[@kind value_or_null & value_or_null]))
         @ portable
      -> length:('s -> int) @ portable
      -> 'a t @ local

    (** [zip_exn seq0 seq1] joins an ['a] sequence and a ['b] sequence into a [('a * 'b)]
        sequence by pairing each element. Raises if the two sequences do not have equal
        lengths. *)
    val zip_exn : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

    (** [mapi seq ~f] converts an ['a] sequence to a ['b] sequence by applying [f] to each
        element of [seq] and its index. *)
    val mapi
      :  'a t @ local
      -> f:(Parallel_kernel.t @ local -> int -> 'a -> 'b) @ portable
      -> 'b t @ local

    (** [iteri parallel seq ~f] applies [f] to each element of [seq] and its index, in
        parallel. The order in which [f] is applied is unspecified and potentially
        non-deterministic. *)
    val iteri
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(Parallel_kernel.t @ local -> int -> 'a -> unit) @ shareable
      -> unit

    (** [foldi parallel seq ~f ~init ~combine] folds [combine] over [mapi seq ~f] in
        parallel. [combine] and [init] must form a monoid over ['acc]: [combine] must be
        associative and [init] must be a neutral element. The order in which [f] and
        [combine] are applied is unspecified and potentially non-deterministic. *)
    val foldi
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> init:(unit -> 'acc) @ portable
      -> f:(Parallel_kernel.t @ local -> int -> 'acc -> 'a -> 'acc) @ shareable
      -> combine:(Parallel_kernel.t @ local -> 'acc -> 'acc -> 'acc) @ shareable
      -> 'acc

    (** [findi parallel seq ~f] returns the first element of [seq], along with its index
        [i], for which [f] returns [true], if it exists. [f] will always be applied to
        every element of [seq]. The order in which [f] is applied is unspecified and
        potentially non-deterministic. *)
    val findi
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(Parallel_kernel.t @ local -> int -> 'a -> bool) @ shareable
      -> (int * 'a) option
  end

  (** [of_with_length seq] converts a sequence with a known length to a sequence with a
      potentially unknown length. However, the known length will be preserved when
      possible, so collecting the sequence may be equally efficient. *)
  val of_with_length : 'a With_length.t @ local -> 'a t @ local
end
