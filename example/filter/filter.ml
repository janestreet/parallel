open! Core
module Capsule = Await.Capsule
module Parallel_array = Parallel.Arrays.Array

[@@@disable_unused_warnings]

(* $MDX part-begin=blur_at *)
let blur_at image ~x ~y =
  let width = Image.width image in
  let height = Image.height image in
  let acc = ref 0. in
  let radius = 4 in
  for i = -radius to radius do
    for j = -radius to radius do
      let x = Int.clamp_exn (x + i) ~min:0 ~max:(width - 1) in
      let y = Int.clamp_exn (y + j) ~min:0 ~max:(height - 1) in
      acc := !acc +. Image.get image ~x ~y
    done
  done;
  !acc /. Float.of_int (((2 * radius) + 1) * ((2 * radius) + 1))
;;

(* $MDX part-end *)

(* $MDX part-begin=filter-mutex *)
let filter ~scheduler ~mutex image =
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    let width = Image.width (Capsule.Data.get_id image) in
    let height = Image.height (Capsule.Data.get_id image) in
    let data =
      Parallel_array.init parallel (width * height) ~f:(fun _ i ->
        let x = i % width in
        let y = i / width in
        Await_blocking.with_await Await.Terminator.never ~f:(fun await ->
          Capsule.Mutex.with_lock await mutex ~f:(fun access ->
            let image = Capsule.Data.unwrap image ~access in
            blur_at image ~x ~y)))
    in
    Image.of_array (Parallel_array.to_array data) ~width ~height)
;;

(* $MDX part-end *)

(* $MDX part-begin=filter-key *)
let filter ~scheduler ~key image =
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    let width = Image.width (Capsule.Data.get_id image) in
    let height = Image.height (Capsule.Data.get_id image) in
    let data =
      Parallel_array.init parallel (width * height) ~f:(fun _ i ->
        let x = i % width in
        let y = i / width in
        (Capsule.Expert.Key.access_shared key ~f:(fun access ->
           { aliased = blur_at (Capsule.Expert.Data.unwrap_shared image ~access) ~x ~y }))
          .aliased)
    in
    Image.of_array (Parallel_array.to_array data) ~width ~height)
;;

(* $MDX part-end *)

let command =
  Command.basic
    ~summary:"filter an image"
    [%map_open.Command
      let file = anon (maybe_with_default "ox.pgm" ("FILE" %: string))
      and max_domains =
        flag "max-domains" (optional int) ~doc:"INT maximum domain count"
      in
      fun () ->
        let scheduler = Parallel_scheduler.create ?max_domains () in
        let (P key) = Capsule.Expert.create () in
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
