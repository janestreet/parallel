@@ portable

open! Base
open! Import
module Thunk := Parallel_kernel1.Thunk

include module type of struct
  include Parallel_kernel0.Runqueue
end

val add_tokens : t @ local once -> int -> unit
val promote : t @ local once -> scheduler:Parallel_kernel0.Scheduler.t -> unit

val with_jobs
  : ('a : value mod portable) 'b.
  t @ local
  -> 'a Thunk.t @ local once
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable
  -> Parallel_kernel1.t @ local
  -> 'a Result.t * ('b * 'l) Hlist.Gen(Result.Capsule).t @ forkable local portable unique

module For_testing : sig
  val create : unit -> t @ local
  val tokens : t @ local -> int

  val with_jobs
    :  t @ local
    -> ('l * 'll) Hlist.Gen(Thunk).t @ once portable
    -> f:(t @ local -> unit) @ local
    -> unit

  val promote : t @ local -> n:int -> f:(tokens:int -> unit) @ local -> unit
end
