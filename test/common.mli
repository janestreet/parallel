open! Base
open! Import

module type Empty = sig end

module Empty : Empty

module Test_schedulers (Test_scheduler : functor (_ : Parallel.Scheduler.S) -> Empty) :
  Empty
