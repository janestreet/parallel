@@ portable

open! Base
open! Import

include module type of struct
  include Parallel_kernel0.Parallel
end

module Dynamic : sig
  type 'a t : immutable_data

  val key : Parallel_kernel0.Parallel.t Stack_pointer.Imm.t t
end

val with_parallel
  :  (t @ local -> 'a @ local portable unique) @ local once portable
  -> scheduler:Parallel_kernel0.Scheduler.t
  -> tokens:int
  -> password:'k Capsule.Password.t @ local
  -> handler:Parallel_kernel0.Wait.t Handled_effect.Handler.t @ local portable
  -> 'a @ local portable unique

val handler_exn
  :  t @ local
  -> Parallel_kernel0.Wait.t Handled_effect.Handler.t @ contended local portable

module Thunk : sig
  include module type of struct
    include Parallel_kernel0.Thunk
  end

  val apply
    :  'a t @ local once
    -> Parallel_kernel0.Parallel.t @ local
    -> 'a Result.t @ local unique

  val encapsulate
    :  'a t @ local once portable
    -> Parallel_kernel0.Parallel.t @ local
    -> 'a Result.Capsule.t @ local unique
end

module Job : sig
  include module type of struct
    include Parallel_kernel0.Job
  end

  val wrap : 'a Thunk.t @ once portable -> 'a t @ once portable
end
