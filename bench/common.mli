open! Base

module type Empty = sig end

module type Scheduler = sig
  val parallel : (Parallel.t @ local -> 'a) @ once shareable -> 'a
end

module Sequential : Scheduler
module Parallel : Scheduler
module Bench_schedulers (Bench_scheduler : functor (_ : Scheduler) -> Empty) : Empty
