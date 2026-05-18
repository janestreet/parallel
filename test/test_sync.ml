open! Core
open! Await

module Table = struct
  type ('k : value mod contended portable, 'v) t =
    ('k, 'v Parallel.Lazy.t) Hashtbl.t Capsule.Sync.With_mutex.t

  let create
    (type k : value mod contended portable)
    (module K : Hashtbl.Key with type t = k)
    =
    Capsule.Sync.With_mutex.create (fun () -> Hashtbl.create (module K))
  ;;

  let find_or_insert parallel (t : ('k, _) t) (k : 'k) f =
    Capsule.Sync.With_mutex.with_lock (Parallel.sync parallel) t ~f:(fun _ t ->
      match Hashtbl.find t k with
      | Some lazy_ -> lazy_
      | None ->
        let lazy_ = Parallel.Lazy.from_fun (fun parallel -> f parallel k) in
        Hashtbl.set t ~key:k ~data:lazy_;
        lazy_)
    [@nontail]
  ;;

  let memo parallel t k ~f = find_or_insert parallel t k f |> Parallel.Lazy.force parallel
end

module Matrix = struct
  type t =
    { width : int
    ; height : int
    }
end

module Index = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

let min_cost parallel (matrices : Matrix.t iarray) =
  let table = Table.create (module Index) in
  let rec aux parallel ~i ~j =
    Table.memo parallel table (i, j) ~f:(fun parallel (i, j) ->
      Parallel.Sequence.range i j
      |> Parallel.Sequence.map ~f:(fun parallel pivot ->
        let l = aux parallel ~i ~j:pivot in
        let r = aux parallel ~i:(pivot + 1) ~j in
        let cost =
          matrices.:(i).width * matrices.:(pivot).height * matrices.:(j).height
        in
        cost + l + r)
      |> Parallel.Sequence.reduce parallel ~f:(fun _ a b -> Int.min a b)
      |> Option.value ~default:0)
    [@nontail]
  in
  aux parallel ~i:0 ~j:(Iarray.length matrices - 1)
;;

module Test_scheduler (Scheduler : Common.Scheduler) = struct
  let%expect_test "concurrency overload" =
    let matrices =
      let heights = Iarray.init 100 ~f:(fun i -> (i % 10) + 1) in
      Iarray.init 100 ~f:(fun i : Matrix.t ->
        if i = 0
        then { width = 10; height = heights.:(i) }
        else { width = heights.:(i - 1); height = heights.:(i) })
    in
    Scheduler.parallel (fun parallel -> printf "%d\n" (min_cost parallel matrices));
    [%expect {| 3408 |}]
  ;;
end

include Common.Test_schedulers (Test_scheduler)
