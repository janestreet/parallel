@@ portable

open! Base
open! Import

include module type of struct
  include Parallel_kernel0.Parallel
end

(** [heartbeat_counter] contains the number of heartbeat intervals elapsed since the first
    call to [create]. It is incremented every [Env.heartbeat_interval_us] microseconds by
    a background thread. *)
val heartbeat_counter : int Atomic.t

val create_sequential : Panic.Monitor.t -> t @ local

val create_parallel
  :  scheduler:Parallel_kernel0.Scheduler.t
  -> password:'k Capsule.Password.t @ local
  -> handler:Parallel_kernel0.Await.t Effect.Handler.t @ local portable
  -> t @ local

val monitor : t @ local -> Panic.Monitor.t

val handler_exn
  :  t @ local
  -> Parallel_kernel0.Await.t Effect.Handler.t @ contended local portable

module Thunk : sig
  include module type of struct
    include Parallel_kernel0.Thunk
  end

  val apply
    :  'a t @ local once portable
    -> Parallel_kernel0.Parallel.t @ local
    -> 'a Panic.Result.t @ local unique
end

module Job : sig
  include module type of struct
    include Parallel_kernel0.Job
  end

  val wrap : 'a Thunk.t @ once portable -> 'a t @ once portable
end
