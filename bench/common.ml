open! Base

module type Empty = sig end

module type Scheduler = sig
  val parallel : (Parallel.t @ local -> 'a) @ once shareable -> 'a
end

module Sequential : Scheduler = struct
  let[@inline] parallel f = f Parallel.sequential
end

module Parallel : Scheduler = struct
  let t = Parallel_scheduler.scheduler ~on_root:Concurrent_in_thread.scheduler ()
  let[@inline] parallel f = Parallel_scheduler.parallel t (fun [@inline] par _ -> f par)
end

module Bench_schedulers (Bench_scheduler : functor (_ : Scheduler) -> Empty) = struct
  module%bench Bench_sequential = Bench_scheduler (Sequential)
  module%bench Bench_parallel = Bench_scheduler (Parallel)
end
