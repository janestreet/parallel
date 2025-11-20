open! Base
open! Import
module Parallel = Parallel_kernel0.Parallel
module Runqueue = Parallel_kernel0.Runqueue
include Parallel_kernel0.Parallel

module Dynamic = struct
  type 'a t : immutable_data

  external create
    :  Parallel.t Stack_pointer.Imm.t
    -> Parallel.t Stack_pointer.Imm.t t
    @@ portable
    = "parallel_create_dynamic"

  external unsafe_set_fiber
    :  Parallel.t Stack_pointer.Imm.t t
    -> Parallel.t Stack_pointer.Imm.t
    -> unit
    @@ portable
    = "parallel_unsafe_set_dynamic"

  let key = create Stack_pointer.Imm.null

  (* Assumes that the current fiber does not have a dynamic binding. This is always the
     case in [with_parallel] because it is called exactly once at the top level of each
     promoted task. The queue pointed to by [ptr] must be stack allocated and must outlive
     the application of [f]. *)
  let[@inline] unsafe_with ~ptr f = exclave_
    unsafe_set_fiber key ptr;
    let result = f () in
    unsafe_set_fiber key Stack_pointer.Imm.null;
    result
  ;;
end

let with_parallel f ~scheduler ~tokens ~password ~handler = exclave_
  let queue =
    Capsule.Data.Local.create (fun [@inline] () : Runqueue.t -> exclave_
      stack_
        { tokens; head = Q (Stack_pointer.null ()); cursor = Q (Stack_pointer.null ()) })
  in
  let t = stack_ Parallel { password; queue; handler; scheduler } in
  (* Assure [t] outlives the application of [f]. *)
  Stack_pointer.unsafe_with_value t ~f:(fun [@inline] ptr -> exclave_
    let ptr = Stack_pointer.Imm.of_ptr ptr in
    Dynamic.unsafe_with ~ptr (fun [@inline] () -> exclave_ f t) [@nontail])
  [@nontail]
;;

let[@inline] handler_exn = function
  | Sequential -> failwith "sequential schedulers have no effect handler"
  | Parallel { handler; _ } -> handler
;;

module Thunk = struct
  include Parallel_kernel0.Thunk

  let[@inline] apply f parallel = exclave_
    Result.try_with (fun [@inline] () -> f parallel)
  ;;

  let[@inline] encapsulate f parallel = exclave_
    Result.Capsule.try_with (fun [@inline] () -> f parallel)
  ;;
end

module Job = struct
  include Parallel_kernel0.Job

  let[@inline] wrap f parallel = exclave_ Thunk.encapsulate f parallel
end
