open! Base
module Capsule = Portable.Capsule.Expert
module Par_array = Parallel.Arrays.Array
module Slice = Par_array.Slice

let swap slice ~i ~j =
  let temp = Slice.get slice i in
  Slice.set slice i (Slice.get slice j);
  Slice.set slice j temp
;;

let partition slice =
  let length = Slice.length slice in
  let pivot = Random.int length in
  swap slice ~i:pivot ~j:(length - 1);
  let pivot = Slice.get slice (length - 1) in
  let store = ref 0 in
  for i = 0 to length - 2 do
    if Slice.get slice i <= pivot
    then (
      swap slice ~i ~j:!store;
      Int.incr store)
  done;
  swap slice ~i:!store ~j:(length - 1);
  !store
;;

module Sequential = struct
  let rec quicksort slice =
    if Slice.length slice > 1
    then (
      let pivot = partition slice in
      let left = Slice.sub slice ~i:0 ~j:pivot in
      let right = Slice.sub slice ~i:pivot ~j:(Slice.length slice) in
      quicksort left;
      quicksort right [@nontail])
  ;;

  let%bench_fun "sequential" =
    let array = Array.init 10_000 ~f:(fun _ -> Random.int 10_000) |> Par_array.of_array in
    fun () -> quicksort (Slice.slice array) [@nontail]
  ;;
end

module Parallel = struct
  let rec quicksort parallel slice =
    if Slice.length slice > 1
    then (
      let pivot = partition slice in
      let (), () =
        Slice.fork_join2
          parallel
          ~pivot
          slice
          (fun parallel left -> quicksort parallel left)
          (fun parallel right -> quicksort parallel right)
      in
      ())
  ;;

  let quicksort ~scheduler ~mutex array =
    let monitor = Parallel.Monitor.create_root () in
    Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
      Capsule.Mutex.with_lock mutex ~f:(fun password ->
        Capsule.Data.iter array ~password ~f:(fun array ->
          let array = Par_array.of_array array in
          quicksort parallel (Slice.slice array) [@nontail])
        [@nontail])
      [@nontail])
  ;;

  let%bench_fun "parallel" =
    let domains = Sys.getenv "DOMAINS" |> Option.bind ~f:Int.of_string_opt in
    let scheduler =
      (Parallel_scheduler_work_stealing.create [@alert "-experimental"]) ?domains ()
    in
    let (P key) = Capsule.create () in
    let mutex = Capsule.Mutex.create key in
    let array =
      Capsule.Data.create (fun () -> Array.init 10_000 ~f:(fun _ -> Random.int 10_000))
    in
    fun () -> quicksort ~scheduler ~mutex array
  ;;
end
