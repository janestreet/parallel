open Parallel

external runtime5 : unit -> bool = "%runtime5"
external caml_fatal_error : string -> unit = "caml_fatal_error"

let () =
  if runtime5 ()
  then (
    let monitor = Monitor.create_root () in
    let scheduler = (Parallel_scheduler_stack.create [@alert "-experimental"]) () in
    Parallel_scheduler_stack.Expert.schedule_async
      scheduler
      ~monitor
      ~on_panic:(fun _panic -> failwith "on_panic")
      ~f:(fun _ -> failwith "panic");
    Parallel_scheduler_stack.stop scheduler)
  else caml_fatal_error "Panicked in on_panic, terminating program."
;;
