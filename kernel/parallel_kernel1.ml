open! Base
open! Import
include Parallel_kernel0.Parallel

external heartbeat_counter
  :  unit
  -> int Atomic.t
  @@ portable
  = "parallel_heartbeat_counter"

external start_heartbeating
  :  interval_us:int
  -> unit
  @@ portable
  = "parallel_start_heartbeating"

let heartbeat_counter = heartbeat_counter ()
let[@inline] create_sequential monitor = exclave_ Sequential monitor

let[@inline] create_parallel
  ~(scheduler : Parallel_kernel0.Scheduler.t)
  ~password
  ~handler
  = exclave_
  let heartbeats = Atomic.get heartbeat_counter in
  if heartbeats = -1 then start_heartbeating ~interval_us:Env.heartbeat_interval_us;
  let queue =
    Capsule.Data.Local.create (fun () : Parallel_kernel0.Runqueue.t ->
      exclave_
      { promote = scheduler.#promote
      ; wake = scheduler.#wake
      ; head = Q (Stack_pointer.null ())
      ; cursor = Q (Stack_pointer.null ())
      ; heartbeats = max heartbeats 0
      })
  in
  Parallel { monitor = scheduler.#monitor; password; queue; handler }
;;

let[@inline] monitor (Sequential monitor | Parallel { monitor; _ }) = monitor

let[@inline] handler_exn = function
  | Sequential _ -> failwith "sequential schedulers have no effect handler"
  | Parallel { handler; _ } -> handler
;;

module Thunk = struct
  include Parallel_kernel0.Thunk

  let[@inline] apply f parallel = exclave_
    Panic.Result.handle_panics_and_report_exceptions
      (monitor parallel)
      (fun [@inline] () -> f parallel)
  ;;
end

module Job = struct
  include Parallel_kernel0.Job

  let[@inline] wrap f parallel = exclave_ Thunk.apply f parallel
end
