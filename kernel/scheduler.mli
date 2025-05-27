@@ portable

open! Base
open! Import

include module type of struct
  include Parallel_kernel0.Scheduler
end

val root
  :  t
  -> f:unit Parallel_kernel1.Thunk.t @ once portable
  -> (unit -> unit) @ once portable

val promote : t -> queue:Runqueue.t @ local -> unit

module One_job : sig
  type 'a t : value & value

  val await
    :  'a t @ once portable
    -> Parallel_kernel1.t @ local
    -> 'a Panic.Result.t @ local unique
end

val promote_one
  :  t
  -> f:'a Parallel_kernel1.Thunk.t @ once portable
  -> 'a One_job.t @ once portable
