@@ portable

open! Base
open! Import
module Job := Parallel_kernel1.Job
module Thunk := Parallel_kernel1.Thunk

include module type of struct
  include Parallel_kernel0.Runqueue
end

val promote
  :  t @ local
  -> f:('a. 'a Job.t @ once portable -> 'a Promise.t -> unit) @ local
  -> unit

val with_jobs
  :  t @ local
  -> 'a Thunk.t @ once portable
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable
  -> Parallel_kernel1.t @ local
  -> 'a Panic.Result.t * ('b * 'l) Hlist.Gen(Panic.Result).t
     @ contended local portable unique

module For_testing : sig
  val create : unit -> t @ local

  val with_jobs
    :  t @ local
    -> ('l * 'll) Hlist.Gen(Thunk).t @ once portable
    -> f:(t @ local -> unit) @ local
    -> unit

  val promote : t @ local -> n:int -> f:(unit -> unit) @ local -> unit
end
