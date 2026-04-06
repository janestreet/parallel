open! Base
module Arrays = Parallel_arrays
module Map = Parallel_map
module Sequence = Parallel_sequence
module Capsule = Parallel_capsule

(** @inline *)
include module type of struct
  include Parallel_kernel
end

module Lazy : Await_sync.Expert.Lazy.S with type capability := t
