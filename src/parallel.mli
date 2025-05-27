open! Base
module Sequence = Parallel_sequence
module Arrays = Parallel_arrays

(** @inline *)
include module type of struct
  include Parallel_kernel
end
