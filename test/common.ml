open! Base
open! Import

module type Empty = sig end

module Empty = struct end

module type Scheduler = sig
  include Parallel.Scheduler.S

  val configure : 'k create_fn -> 'k
end

module Test_schedulers (Test_scheduler : functor (_ : Scheduler) -> Empty) = struct
  module Test_sequential = Test_scheduler (struct
      include Parallel.Scheduler.Sequential

      type 'k create_fn = 'k

      let configure create_fn = create_fn
    end)

  module Test_queue =
    (val if is_runtime5
         then
           (module Test_scheduler (struct
               include Parallel_scheduler_stack

               type 'k create_fn = ?domains:int -> 'k

               let configure create_fn = create_fn ?domains:(Some 4)
             end) : Empty)
         else (module Empty))

  module Test_work_stealing =
    (val if is_runtime5
         then
           (module Test_scheduler (struct
               include Parallel_scheduler_work_stealing

               type 'k create_fn = ?domains:int -> 'k

               let configure create_fn = create_fn ?domains:(Some 4)
             end) : Empty)
         else (module Empty))
end
