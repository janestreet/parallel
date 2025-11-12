open Base
module Scheduler = Parallel_scheduler

let max_domains =
  [ 128; 64; 32; 16; 8; 4; 2; 1 ]
  |> List.filter ~f:(fun max_domains -> max_domains <= Multicore.max_domains ())
;;

let%bench ("scheduler creation" [@indexed max_domains = max_domains]) =
  let scheduler = Scheduler.create ~max_domains () in
  Scheduler.parallel scheduler ~f:(fun _parallel -> ());
  Scheduler.stop scheduler
;;
