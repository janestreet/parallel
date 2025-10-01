@@ portable

open! Base
open! Import
module Thunk := Parallel_kernel1.Thunk

include module type of struct
  include Parallel_kernel0.Runqueue
end

val promote : t @ local once -> add_tokens:int -> unit

val with_jobs
  : ('a : value mod portable) 'b.
  t @ local
  -> 'a Thunk.t @ local once
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable
  -> Parallel_kernel1.t @ local
  -> 'a Result.t * ('b * 'l) Hlist.Gen(Result.Capsule).t
     @ local portable unique unyielding

module For_testing : sig
  val create : unit -> t @ local

  val with_jobs
    :  t @ local
    -> ('l * 'll) Hlist.Gen(Thunk).t @ once portable
    -> f:(t @ local -> unit) @ local
    -> unit

  val promote : t @ local -> n:int -> f:(unit -> unit) @ local -> unit
end
