open! Base
open! Import

module type Empty = sig end

module Empty = struct end

module Test_schedulers (Test_scheduler : functor (_ : Parallel.Scheduler.S) -> Empty) =
struct
  module Test_sequential = Test_scheduler (Parallel.Scheduler.Sequential)
  module Test_work_stealing = Test_scheduler (Parallel_scheduler_work_stealing)
end
