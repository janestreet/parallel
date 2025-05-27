open! Core
module Capsule = Portable.Capsule.Expert
module Parallel_array = Parallel.Arrays.Array

let blur_at image ~x ~y =
  let width = Image.width image in
  let height = Image.height image in
  let acc = ref 0. in
  for i = -4 to 4 do
    for j = -4 to 4 do
      let x = Int.clamp_exn (x + i) ~min:0 ~max:(width - 1) in
      let y = Int.clamp_exn (y + j) ~min:0 ~max:(height - 1) in
      acc := !acc +. Image.get image ~x ~y
    done
  done;
  !acc /. 81.
;;

let filter ~scheduler ~key image =
  let monitor = Parallel.Monitor.create_root () in
  Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
    let width = Image.width (Capsule.Data.project image) in
    let height = Image.height (Capsule.Data.project image) in
    let data =
      Parallel_array.init parallel (width * height) ~f:(fun i ->
        let x = i % width in
        let y = i / width in
        (Capsule.Key.access_shared key ~f:(fun access ->
           { aliased = blur_at (Capsule.Data.unwrap_shared image ~access) ~x ~y }))
          .aliased)
    in
    Image.of_array (Parallel_array.to_array data) ~width ~height)
;;

let command =
  Command.basic
    ~summary:"filter an image"
    [%map_open.Command
      let file = anon (maybe_with_default "ox.pgm" ("FILE" %: string))
      and domains = flag "domains" (optional int) ~doc:"INT number of domains" in
      fun () ->
        let scheduler =
          (Parallel_scheduler_work_stealing.create [@alert "-experimental"]) ?domains ()
        in
        let (P key) = Capsule.create () in
        let image = Capsule.Data.create (fun () -> Image.load file) in
        let start = Time_stamp_counter.now () in
        let result = filter ~scheduler ~key image in
        let finish = Time_stamp_counter.now () in
        printf
          "Completed in %dms.\n"
          (Time_stamp_counter.Span.to_int_exn (Time_stamp_counter.diff finish start)
           / 1_000_000);
        Image.save result ("filtered-" ^ Filename.basename file)]
;;

let () = Command_unix.run command
