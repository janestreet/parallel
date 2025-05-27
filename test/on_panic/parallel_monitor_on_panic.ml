open Parallel

let () =
  let monitor = Monitor.create_root () in
  Monitor.on_panic monitor (fun _incident -> failwith "on_panic");
  let scheduler = Scheduler.Sequential.create () in
  Scheduler.Sequential.schedule scheduler ~monitor ~f:(fun _ -> failwith "panic")
;;
