open! Base

module%bench _ : Parallel.Map.S = struct
  type ('key, 'data, 'cmp) map = ('key, 'data, 'cmp) Map.t
  type ('key, 'cmp, 'fn) with_comparator = 'fn

  open struct
    let parallel = Common.Parallel.parallel
    let ignore x = ignore (Sys.opaque_identity x : _)

    let rec fib = function
      | 0 -> 0
      | 1 -> 1
      | n -> fib (n - 2) + fib (n - 1)
    ;;

    let work () = ignore (Sys.opaque_identity (fib 10) : int)

    let lazy_map1 =
      Lazy.from_fun (fun () ->
        Map.of_increasing_iterator_unchecked (module Int) ~len:100_000 ~f:(fun i -> i, i))
    ;;

    let lazy_map2 =
      Lazy.from_fun (fun () ->
        Map.of_increasing_iterator_unchecked (module Int) ~len:100_000 ~f:(fun i ->
          i + 50_000, i))
    ;;

    let test_seq f =
      let map = force lazy_map1 in
      fun () : unit -> parallel (fun _ -> f map)
    ;;

    let test_par f =
      let map = force lazy_map1 in
      fun () : unit -> parallel (fun par -> f par map)
    ;;

    let test_seq2 f =
      let map1 = force lazy_map1 in
      let map2 = force lazy_map2 in
      fun () : unit -> parallel (fun _ -> f map1 map2)
    ;;

    let test_par2 f =
      let map1 = force lazy_map1 in
      let map2 = force lazy_map2 in
      fun () : unit -> parallel (fun par -> f par map1 map2)
    ;;
  end

  let fold = Parallel.Map.fold

  let%bench_fun "fold (Base.Map)" =
    test_seq (fun map -> Map.fold map ~init:() ~f:(fun ~key:_ ~data:_ () -> work ()))
  ;;

  let%bench_fun "fold (Parallel)" =
    test_par (fun par map ->
      Parallel.Map.fold
        par
        map
        ~init:(fun () -> ())
        ~f:(fun _ ~key:_ ~data:_ () -> work ())
        ~combine:(fun _ () () -> ()))
  ;;

  let iter = Parallel.Map.iter

  let%bench_fun "iter (Base.Map)" =
    test_seq (fun map -> Map.iteri map ~f:(fun ~key:_ ~data:_ -> work ()))
  ;;

  let%bench_fun "iter (Parallel)" =
    test_par (fun par map ->
      Parallel.Map.iter par map ~f:(fun _ ~key:_ ~data:_ -> work ()))
  ;;

  let map = Parallel.Map.map

  let%bench_fun "map (Base.Map)" =
    test_seq (fun map ->
      ignore
        (Map.mapi map ~f:(fun ~key:_ ~data ->
           work ();
           data)
         : (_, _, _) Map.t))
  ;;

  let%bench_fun "map (Parallel)" =
    test_par (fun par map ->
      ignore
        (Parallel.Map.map par map ~f:(fun _ ~key:_ ~data ->
           work ();
           data)
         : (_, _, _) Map.t))
  ;;

  let filter = Parallel.Map.filter

  let%bench_fun "filter (Base.Map)" =
    test_seq (fun map ->
      ignore
        (Map.filteri map ~f:(fun ~key:_ ~data ->
           work ();
           data land 1 = 1)
         : (_, _, _) Map.t))
  ;;

  let%bench_fun "filter (Parallel)" =
    test_par (fun par map ->
      ignore
        (Parallel.Map.filter par map ~f:(fun _ ~key:_ ~data ->
           work ();
           data land 1 = 1)
         : (_, _, _) Map.t))
  ;;

  let filter_map = Parallel.Map.filter_map

  let%bench_fun "filter_map (Base.Map)" =
    test_seq (fun map ->
      ignore
        (Map.filter_mapi map ~f:(fun ~key:_ ~data ->
           work ();
           if data land 1 = 1 then Some 0 else None)
         : (_, _, _) Map.t))
  ;;

  let%bench_fun "filter_map (Parallel)" =
    test_par (fun par map ->
      ignore
        (Parallel.Map.filter_map par map ~f:(fun _ ~key:_ ~data ->
           work ();
           if data land 1 = 1 then Some 0 else None)
         : (_, _, _) Map.t))
  ;;

  let partition_tf = Parallel.Map.partition_tf

  let%bench_fun "partition_tf (Base.Map)" =
    test_seq (fun map ->
      ignore
        (Map.partitioni_tf map ~f:(fun ~key:_ ~data ->
           work ();
           data land 1 = 1)
         : (_, _, _) Map.t * (_, _, _) Map.t))
  ;;

  let%bench_fun "partition_tf (Parallel)" =
    test_par (fun par map ->
      ignore
        (Parallel.Map.partition_tf par map ~f:(fun _ ~key:_ ~data ->
           work ();
           data land 1 = 1)
         : (_, _, _) Map.t * (_, _, _) Map.t))
  ;;

  let partition_map = Parallel.Map.partition_map

  let%bench_fun "partition_map (Base.Map)" =
    test_seq (fun map ->
      ignore
        (Map.partition_mapi map ~f:(fun ~key:_ ~data ->
           work ();
           if data land 1 = 1 then First 0 else Second 1)
         : (_, _, _) Map.t * (_, _, _) Map.t))
  ;;

  let%bench_fun "partition_map (Parallel)" =
    test_par (fun par map ->
      ignore
        (Parallel.Map.partition_map par map ~f:(fun _ ~key:_ ~data ->
           work ();
           if data land 1 = 1 then First 0 else Second 1)
         : (_, _, _) Map.t * (_, _, _) Map.t))
  ;;

  let merge_filter_map = Parallel.Map.merge_filter_map

  let%bench_fun "merge_filter_map (Base.Map)" =
    test_seq2 (fun map1 map2 ->
      ignore
        (Map.merge map1 map2 ~f:(fun ~key _ ->
           work ();
           if key land 1 = 1 then Some 0 else None)
         : (_, _, _) Map.t))
  ;;

  let%bench_fun "merge_filter_map (Parallel)" =
    test_par2 (fun par map1 map2 ->
      ignore
        (Parallel.Map.merge_filter_map par map1 map2 ~f:(fun _ ~key _ ->
           work ();
           if key land 1 = 1 then Some 0 else None)
         : (_, _, _) Map.t))
  ;;

  let to_sequence = Parallel.Map.to_sequence
  let to_sequence2 = Parallel.Map.to_sequence2
  let traverse = Parallel.Map.traverse
end
