open! Base
open Await
module Arrays = Parallel_arrays
module Map = Parallel_map
module Sequence = Parallel_sequence
module Capsule = Parallel_capsule
include Parallel_kernel

module Lazy = Await_sync.Expert.Lazy.Make (struct
    type t = Parallel_kernel.t

    let unsafe_to_await par = exclave_
      (Await.Expert.create [@alloc stack])
        ~sync:(Parallel_kernel.sync par)
        ~terminator:Terminator.never
    ;;
  end)
