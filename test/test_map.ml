open! Core
open Quickcheck
open Expect_test_helpers_core

module Test (Scheduler : Common.Scheduler) : Parallel.Map.S = struct
  type ('key, 'data, 'cmp) map = ('key, 'data, 'cmp) Map.t
  type ('key, 'cmp, 'fn) with_comparator = 'fn

  open struct
    (* Test helpers, not for export. *)

    let quickcheck_config : Base_quickcheck.Test.Config.t =
      (* Rather than 10k maps of small size, generate 1k maps of size up to 1k. This way
         we test large enough inputs that the work actually parallelizes. *)
      { seed = Base_quickcheck.Test.default_config.seed
      ; test_count = 100
      ; shrink_count = 1_000_000
      ; sizes = Sequence.unfold ~init:0 ~f:(fun n -> Some (n, n + 1))
      }
    ;;

    let test
      (type (a : value mod contended portable) b)
      (module Input : Base_quickcheck.Test.S with type t = a)
      (module Output : With_equal with type t = b)
      seq_f
      par_f
      =
      quickcheck_m (module Input) ~config:quickcheck_config ~f:(fun input ->
        Scheduler.parallel (fun par ->
          require_equal (module Output) (seq_f input) (par_f par input)))
    ;;

    module Int = struct
      include Int

      let sexp_of_t n = Sexp.Atom (Int.to_string_hum n)
      let quickcheck_generator = Generator.small_positive_int

      let quickcheck_shrinker =
        Shrinker.create (fun n ->
          if n <= 1 then Sequence.empty else Sequence.singleton (n - 1))
      ;;
    end

    type int = Int.t [@@deriving quickcheck ~generator ~shrinker, sexp_of]

    module Int2_alist = struct
      type t = (int * int) list [@@deriving equal, sexp_of]

      (* Use the whole [size] as list length so we get some large maps. Only generates
         keys in sorted order, but that's fine, we don't use them for anything but
         generating maps. *)
      let quickcheck_generator_sorted =
        Generator.create (fun ~size ~random ->
          List.init size ~f:(fun _ ->
            ( Splittable_random.int random ~lo:0 ~hi:size
            , Splittable_random.int random ~lo:0 ~hi:size ))
          |> List.folding_map ~init:0 ~f:(fun min_key (k, data) ->
            let key = min_key + k in
            key + 1, (key, data)))
      ;;
    end

    module Int2_map = struct
      type t = int Map.M(Int).t [@@deriving equal, quickcheck ~shrinker, sexp_of]

      let quickcheck_generator =
        let%map.Generator alist = Int2_alist.quickcheck_generator_sorted in
        Int.Map.of_alist_exn alist
      ;;

      let check t =
        let comparator = Map.comparator t in
        let tree = Map.to_tree t in
        let ordered = Map.Tree.Expert.order_invariants ~comparator tree in
        let balanced = Map.Tree.Expert.balance_invariants tree in
        match ordered && balanced with
        | true -> ()
        | false ->
          let sexp_of_key = Comparator.sexp_of_t comparator in
          print_cr
            [%message
              "invariants violated"
                (ordered : bool)
                (balanced : bool)
                (tree : (key, _, _) Map.Tree.Expert.t)]
      ;;

      (* We put invariant checks in [equal] so that all maps in our output have their
         invariants checked. *)
      let equal a b =
        check a;
        check b;
        equal a b
      ;;
    end

    module Int2_map2 = struct
      type t = Int2_map.t * Int2_map.t [@@deriving equal, quickcheck ~shrinker, sexp_of]

      let quickcheck_generator =
        (* Generate some maps with shared structure and some without. *)
        let generate_independently = [%generator: Int2_map.t * Int2_map.t] in
        let generate_right_from_left =
          let%map.Generator map1 = [%generator: Int2_map.t]
          and alist = Int2_alist.quickcheck_generator_sorted in
          let map2 =
            List.fold alist ~init:map1 ~f:(fun acc (key, data) -> Map.set acc ~key ~data)
          in
          map1, map2
        in
        let generate_left_from_right =
          Generator.map ~f:(fun (map2, map1) -> map1, map2) generate_right_from_left
        in
        [ generate_independently; generate_right_from_left; generate_left_from_right ]
        |> Generator.union
      ;;
    end
  end

  let to_sequence = Parallel.Map.to_sequence

  let%expect_test "to_sequence" =
    test
      (module Int2_map)
      (module Int2_alist)
      (fun map -> Map.to_sequence map |> Sequence.to_list)
      (fun par map ->
        (Parallel.Map.to_sequence map |> Parallel.Sequence.With_length.to_list par)
        [@nontail])
  ;;

  let traverse = Parallel.Map.traverse

  let%expect_test "traverse" =
    test
      (module Int2_map)
      (module Int2_alist)
      (fun map -> Map.to_alist map)
      (fun par map ->
        Parallel.Map.traverse
          par
          map
          ~on_empty:(fun () -> [])
          ~on_data:(fun _ ~key ~data -> [ key, data ])
          ~on_leaf:(fun _ ~key:_ alist -> alist)
          ~on_node:(fun _ ~key:_ mid left right -> left @ mid @ right))
  ;;

  let fold = Parallel.Map.fold

  let%expect_test "fold" =
    let f ~key ~data acc = acc + (key * data) in
    test
      (module Int2_map)
      (module Int)
      (fun map -> Map.fold map ~init:0 ~f)
      (fun par map ->
        Parallel.Map.fold
          par
          map
          ~init:(fun () -> 0)
          ~f:(fun _ -> f)
          ~combine:(fun _ -> ( + )))
  ;;

  let iter = Parallel.Map.iter

  let%expect_test "iter" =
    let with_atomic (f @ local) =
      let atomic = Atomic.make ([] : (int * int) list) in
      f ~f:(fun ~key ~data ->
        Atomic.update atomic ~pure_f:(fun list -> (key, data) :: list));
      List.sort ~compare:[%compare: int * int] (Atomic.get atomic)
    in
    test
      (module Int2_map)
      (module Int2_alist)
      (fun map -> with_atomic (fun ~f -> Map.iteri map ~f))
      (fun par map ->
        with_atomic (fun ~f -> Parallel.Map.iter par map ~f:(fun _ -> f)) [@nontail])
  ;;

  let map = Parallel.Map.map

  let%expect_test "map" =
    let f ~key ~data = (key * data) + 1 in
    test
      (module Int2_map)
      (module Int2_map)
      (fun map -> Map.mapi map ~f)
      (fun par map -> Parallel.Map.map par map ~f:(fun _ -> f))
  ;;

  let filter = Parallel.Map.filter

  let%expect_test "filter" =
    let f ~key ~data = key * data % 2 = 1 in
    test
      (module Int2_map)
      (module Int2_map)
      (fun map -> Map.filteri map ~f)
      (fun par map -> Parallel.Map.filter par map ~f:(fun _ -> f))
  ;;

  let filter_map = Parallel.Map.filter_map

  let%expect_test "filter_map" =
    let f ~key ~data = Option.some_if (key * data % 2 = 1) (key + data) in
    test
      (module Int2_map)
      (module Int2_map)
      (fun map -> Map.filter_mapi map ~f)
      (fun par map -> Parallel.Map.filter_map par map ~f:(fun _ -> f))
  ;;

  let partition_tf = Parallel.Map.partition_tf

  let%expect_test "partition_tf" =
    let f ~key ~data = key * data % 2 = 1 in
    test
      (module Int2_map)
      (module Int2_map2)
      (fun map -> Map.partitioni_tf map ~f)
      (fun par map -> Parallel.Map.partition_tf par map ~f:(fun _ -> f))
  ;;

  let partition_map = Parallel.Map.partition_map

  let%expect_test "partition_map" =
    let f ~key ~data =
      if key * data % 2 = 1 then First (key + data) else Second (key + data)
    in
    test
      (module Int2_map)
      (module Int2_map2)
      (fun map -> Map.partition_mapi map ~f)
      (fun par map -> Parallel.Map.partition_map par map ~f:(fun _ -> f))
  ;;

  let to_sequence2 = Parallel.Map.to_sequence2

  let%expect_test "to_sequence2" =
    test
      (module Int2_map2)
      (module struct
        type t = (int * [ `Both of int * int | `Left of int | `Right of int ]) list
        [@@deriving equal, sexp_of]
      end)
      (fun (map1, map2) ->
        Map.merge map1 map2 ~f:(fun ~key:_ data -> Some data) |> Map.to_alist)
      (fun par (map1, map2) ->
        (Parallel.Map.to_sequence2 map1 map2 |> Parallel.Sequence.to_list par) [@nontail]);
    [%expect {| |}]
  ;;

  let merge_filter_map = Parallel.Map.merge_filter_map

  let%expect_test "merge_filter_map" =
    let f ~key = function
      | `Left data -> Option.some_if (key mod 3 <> 2) data
      | `Right data -> Option.some_if (key mod 3 <> 1) data
      | `Both (data1, data2) -> Option.some_if (key mod 3 <> 0) (data1 + data2)
    in
    test
      (module Int2_map2)
      (module Int2_map)
      (fun (map1, map2) -> Map.merge map1 map2 ~f)
      (fun par (map1, map2) ->
        Parallel.Map.merge_filter_map par map1 map2 ~f:(fun _ -> f));
    [%expect {| |}]
  ;;
end

include Common.Test_schedulers (Test)
