open! Base
open! Import

module type S = sig @@ portable
  (** ['a t] represents a finite sequence of independent computations that produce an
      ['a]. These computations are evaluated in parallel upon collecting the sequence into
      a strict data structure. Parallel sequences are distinguished from normal sequences
      by supporting the [split] operation, which divides a sequence into two parts that
      may be collected in parallel. *)
  type 'a t : value mod contended portable

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

  (** [init n ~f] is a sequence containing [f i] for each i in [0..n-1]. *)
  val init
    :  int
    -> f:(int -> 'a @ contended portable) @ portable unyielding
    -> 'a t @ local

  (** See [init]. Allows nested parallelism. *)
  val init'
    :  int
    -> f:(Parallel_kernel.t @ local -> int -> 'a @ contended portable)
       @ portable unyielding
    -> 'a t @ local

  (** [of_iarray i] is a sequence containing the contents of the iarray [i]. This sequence
      has a known length and may be split in constant time. *)
  val of_iarray : 'a iarray @ contended portable -> 'a t @ local

  (** [append s0 s1] returns a sequence containing the elements of [s0] and then the
      elements of [s1]. *)
  val append : 'a t @ local -> 'a t @ local -> 'a t @ local

  (** [product seq0 seq1] joins an ['a] sequence and a ['b] sequence into a [('a * 'b)]
      sequence by taking the cartesian product of their elements. *)
  val product : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

  (** [map seq ~f] converts an ['a] sequence to a ['b] sequence by applying [f] to each
      element of [seq]. *)
  val map
    :  'a t @ local
    -> f:('a @ contended portable -> 'b @ contended portable) @ portable unyielding
    -> 'b t @ local

  (** See [map]. Allows nested parallelism. *)
  val map'
    :  'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a @ contended portable -> 'b @ contended portable)
       @ portable unyielding
    -> 'b t @ local

  (** [iter parallel seq ~f] applies [f] to each element of [seq] in parallel. The order
      in which [f] is applied is unspecified and potentially non-deterministic. *)
  val iter
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:('a @ contended portable -> unit) @ portable unyielding
    -> unit

  (** See [iter]. Allows nested parallelism. *)
  val iter'
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a @ contended portable -> unit)
       @ portable unyielding
    -> unit

  (** [fold parallel seq ~f ~init ~combine] folds [combine] over [map seq ~f] in parallel.
      [combine] and [init] must form a monoid over ['acc]: [combine] must be associative
      and [init] must be a neutral element. The order in which [f] and [combine] are
      applied is unspecified and potentially non-deterministic. *)
  val fold
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> init:'acc @ contended portable
    -> f:
         ('acc @ contended portable
          -> 'a @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         ('acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable

  (** See [fold]. Allows nested parallelism. *)
  val fold'
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> init:'acc @ contended portable
    -> f:
         (Parallel_kernel.t @ local
          -> 'acc @ contended portable
          -> 'a @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> combine:
         (Parallel_kernel.t @ local
          -> 'acc @ contended portable
          -> 'acc @ contended portable
          -> 'acc @ contended portable)
       @ portable unyielding
    -> 'acc @ contended portable

  (** [reduce parallel seq ~f] reduces [seq] using [f] in parallel. [f] must be
      associative. If the sequence is empty, [reduce] returns [None]. The order in which
      [f] is applied is unspecified and potentially non-deterministic. *)
  val reduce
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:('a @ contended portable -> 'a @ contended portable -> 'a @ contended portable)
       @ portable unyielding
    -> 'a option @ contended portable

  (** See [reduce]. Allows nested parallelism. *)
  val reduce'
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:
         (Parallel_kernel.t @ local
          -> 'a @ contended portable
          -> 'a @ contended portable
          -> 'a @ contended portable)
       @ portable unyielding
    -> 'a option @ contended portable

  (** [find parallel seq ~f] returns the first element of [seq] for which [f] returns
      [true], if it exists. [f] will always be applied to every element of [seq]. The
      order in which [f] is applied is unspecified and potentially non-deterministic. *)
  val find
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:('a @ contended portable -> bool) @ portable unyielding
    -> 'a option @ contended portable

  (** See [find]. Allows nested parallelism. *)
  val find'
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a @ contended portable -> bool)
       @ portable unyielding
    -> 'a option @ contended portable

  (** [to_list seq] collects a sequence into a list by evaluating each element in
      parallel. Requires iterating the sequence sequentially. *)
  val to_list : Parallel_kernel.t @ local -> 'a t @ local -> 'a list @ contended portable

  (** [to_iarray seq] collects a sequence into an iarray by evaluating each element in
      parallel. If the sequence has a known length, does not require iterating the
      sequence sequentially. *)
  val to_iarray
    :  Parallel_kernel.t @ local
    -> 'a t @ local
    -> 'a iarray @ contended portable
end

module type Parallel_sequence = sig @@ portable
  module type S = S

  include S

  (** [unfold ~init ~next ~split] creates a sequence representing the stream of values
      produced by recursively applying [next] to [init].

      [next state] returns either [Some (a,s)], representing the element [a] and the new
      state [s], or [None], representing the end of the sequence.

      [split state] optionally returns two states [s0, s1] such that the concatenation of
      the unfoldings of [s0] and [s1] produces the same elements as the original sequence.
      [split] must return [Some] whenever the sequence contains more than one element. *)
  val unfold
    :  init:'s @ contended portable
    -> next:
         (Parallel_kernel.t @ local
          -> 's @ contended portable
          -> ('a, 's) Pair_or_null.t @ contended portable)
       @ portable unyielding
    -> split:
         (Parallel_kernel.t @ local
          -> 's @ contended portable
          -> ('s, 's) Pair_or_null.t @ contended portable)
       @ portable unyielding
    -> 'a t @ local

  (** [concat seqs] creates a sequence representing the concatenation of all sequences in
      [seqs]. When possible, splitting the resulting sequence is equivalent to splitting
      the top-level input sequence. Otherwise, splitting separates out only the first
      element of [seqs]. *)
  val concat : 'a t t @ local -> 'a t @ local

  (** [concat_map seq ~f] is equivalent to [map seq ~f |> concat]. *)
  val concat_map
    :  'a t @ local
    -> f:('a @ contended portable -> 'b t) @ portable unyielding
    -> 'b t @ local

  (** See [concat_map]. Allows nested parallelism. *)
  val concat_map'
    :  'a t @ local
    -> f:(Parallel_kernel.t @ local -> 'a @ contended portable -> 'b t)
       @ portable unyielding
    -> 'b t @ local

  (** [filter_map seq ~f] converts an ['a] sequence to a ['b] sequence containing only the
      elements of [seq] such that [f] returns [Some b]. *)
  val filter_map
    :  'a t @ local
    -> f:('a @ contended portable -> 'b option @ contended portable) @ portable unyielding
    -> 'b t @ local

  (** See [filter_map]. Allows nested parallelism. *)
  val filter_map'
    :  'a t @ local
    -> f:
         (Parallel_kernel.t @ local
          -> 'a @ contended portable
          -> 'b option @ contended portable)
       @ portable unyielding
    -> 'b t @ local

  (** Sequences of type ['a With_length.t] are guaranteed to have a known length. When the
      length is known, collecting the sequence can be more efficient. Note that [concat]
      and [filter] functions are not supported. *)
  module With_length : sig
    include S

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
      :  init:'s @ contended portable
      -> next:
           (Parallel_kernel.t @ local
            -> 's @ contended portable
            -> ('a, 's) Pair_or_null.t @ contended portable)
         @ portable unyielding
      -> split_at:
           (Parallel_kernel.t @ local
            -> 's @ contended portable
            -> n:int
            -> ('s, 's) Pair_or_null.t @ contended portable)
         @ portable unyielding
      -> length:('s @ contended portable -> int) @ portable
      -> 'a t @ local

    (** [zip_exn seq0 seq1] joins an ['a] sequence and a ['b] sequence into a [('a * 'b)]
        sequence by pairing each element. Raises if the two sequences do not have equal
        lengths. *)
    val zip_exn : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

    (** [mapi seq ~f] converts an ['a] sequence to a ['b] sequence by applying [f] to each
        element of [seq] and its index. *)
    val mapi
      :  'a t @ local
      -> f:(int -> 'a @ contended portable -> 'b @ contended portable)
         @ portable unyielding
      -> 'b t @ local

    (** See [mapi]. Allows nested parallelism. *)
    val mapi'
      :  'a t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> int
            -> 'a @ contended portable
            -> 'b @ contended portable)
         @ portable unyielding
      -> 'b t @ local

    (** [iteri parallel seq ~f] applies [f] to each element of [seq] and its index, in
        parallel. The order in which [f] is applied is unspecified and potentially
        non-deterministic. *)
    val iteri
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(int -> 'a @ contended portable -> unit) @ portable unyielding
      -> unit

    (** See [iteri]. Allows nested parallelism. *)
    val iteri'
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(Parallel_kernel.t @ local -> int -> 'a @ contended portable -> unit)
         @ portable unyielding
      -> unit

    (** [foldi parallel seq ~f ~init ~combine] folds [combine] over [mapi seq ~f] in
        parallel. [combine] and [init] must form a monoid over ['acc]: [combine] must be
        associative and [init] must be a neutral element. The order in which [f] and
        [combine] are applied is unspecified and potentially non-deterministic. *)
    val foldi
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> init:'acc @ contended portable
      -> f:
           (int
            -> 'acc @ contended portable
            -> 'a @ contended portable
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
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> init:'acc @ contended portable
      -> f:
           (Parallel_kernel.t @ local
            -> int
            -> 'acc @ contended portable
            -> 'a @ contended portable
            -> 'acc @ contended portable)
         @ portable unyielding
      -> combine:
           (Parallel_kernel.t @ local
            -> 'acc @ contended portable
            -> 'acc @ contended portable
            -> 'acc @ contended portable)
         @ portable unyielding
      -> 'acc @ contended portable

    (** [findi parallel seq ~f] returns the first element of [seq], along with its index
        [i], for which [f] returns [true], if it exists. [f] will always be applied to
        every element of [seq]. The order in which [f] is applied is unspecified and
        potentially non-deterministic. *)
    val findi
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(int -> 'a @ contended portable -> bool) @ portable unyielding
      -> (int * 'a) option @ contended portable

    (** See [findi]. Allows nested parallelism. *)
    val findi'
      :  Parallel_kernel.t @ local
      -> 'a t @ local
      -> f:(Parallel_kernel.t @ local -> int -> 'a @ contended portable -> bool)
         @ portable unyielding
      -> (int * 'a) option @ contended portable
  end

  (** [of_with_length seq] converts a sequence with a known length to a sequence with a
      potentially unknown length. However, the known length will be preserved when
      possible, so collecting the sequence may be equally efficient. *)
  val of_with_length : 'a With_length.t @ local -> 'a t @ local
end
