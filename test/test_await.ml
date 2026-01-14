open! Core
open! Await
open Parallel

module Table = struct
  type ('k : value mod contended portable, 'v) t =
    ('k, 'v Ivar.t) Hashtbl.t Capsule.With_mutex.t

  let create
    (type k : value mod contended portable)
    (module K : Hashtbl.Key with type t = k)
    =
    Capsule.With_mutex.create (fun () -> Hashtbl.create (module K))
  ;;

  let find_or_insert await (t : ('k, _) t) (k : 'k) =
    Capsule.With_mutex.with_lock await t ~f:(fun t ->
      match Hashtbl.find t k with
      | Some ivar -> `Found ivar
      | None ->
        let ivar = Ivar.create () in
        Hashtbl.set t ~key:k ~data:ivar;
        `Inserted ivar)
  ;;

  let memo await t k ~f =
    match find_or_insert await t k with
    | `Found ivar -> Ivar.read await ivar
    | `Inserted ivar ->
      let v = f k in
      Ivar.fill_exn ivar v;
      v
  ;;
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

let with_await parallel ~f = exclave_
  Await.with_
    parallel
    ~terminator:Terminator.never
    ~yield:Null
    ~await:Parallel.For_scheduler.await
    ~f
;;

let min_cost parallel (matrices : Matrix.t iarray) =
  let table = Table.create (module Index) in
  let rec aux parallel ~i ~j =
    with_await parallel ~f:(fun await ->
      Table.memo await table (i, j) ~f:(fun (i, j) ->
        Sequence.range i j
        |> Sequence.map ~f:(fun parallel pivot ->
          let l = aux parallel ~i ~j:pivot in
          let r = aux parallel ~i:(pivot + 1) ~j in
          let cost =
            matrices.:(i).width * matrices.:(pivot).height * matrices.:(j).height
          in
          cost + l + r)
        |> Sequence.reduce parallel ~f:(fun _ a b -> Int.min a b)
        |> Option.value ~default:0)
      [@nontail])
    [@nontail]
  in
  aux parallel ~i:0 ~j:(Iarray.length matrices - 1)
;;

let scheduler = Parallel_scheduler.create ()

let%expect_test "concurrency overload" =
  let matrices =
    let heights = Iarray.init 100 ~f:(fun i -> (i % 10) + 1) in
    Iarray.init 100 ~f:(fun i : Matrix.t ->
      if i = 0
      then { width = 10; height = heights.:(i) }
      else { width = heights.:(i - 1); height = heights.:(i) })
  in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
    printf "%d\n" (min_cost parallel matrices));
  [%expect {| 3408 |}]
;;
