open! Base
open! Import

module type Empty = sig end

module Empty = struct end

module Test_schedulers (Test_scheduler : functor (_ : Parallel.Scheduler.S) -> Empty) =
struct
  module _ = Test_scheduler (Parallel.Scheduler.Sequential)
  module _ = Test_scheduler (Parallel_scheduler)
end
