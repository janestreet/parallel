open! Base
open! Import

module type Empty = sig end

module type Scheduler = sig
  val parallel : (Parallel.t @ local -> 'a) @ once shareable -> 'a
end

module Sequential : Scheduler = struct
  let parallel f = f Parallel.sequential
end

module Parallel : Scheduler = struct
  let t = Parallel_scheduler.scheduler ()
  let parallel f = Parallel_scheduler.parallel t (fun par _ -> f par)
end

module With_parallel : Scheduler = struct
  let parallel f = Parallel_scheduler.with_parallel f
end

module Test_schedulers (Test_scheduler : functor (_ : Scheduler) -> Empty) = struct
  module _ = Test_scheduler (Sequential)
  module _ = Test_scheduler (Parallel)
  module _ = Test_scheduler (With_parallel)
end
