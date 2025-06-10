open! Core
open! Import
module Arrays = Parallel.Arrays

let require_raise =
  Expect_test_helpers_core.require_does_raise
  |> Portability_hacks.magic_portable__needs_base_and_core
;;

let assert_eq =
  [%test_result: int array] |> Portability_hacks.magic_portable__needs_base_and_core
;;

let assert_eq2 =
  [%test_result: (int * int) array]
  |> Portability_hacks.magic_portable__needs_base_and_core
;;

module Test_scheduler (Scheduler : Common.Scheduler) = struct
  let monitor = Parallel.Monitor.create_root ()
  let scheduler = Scheduler.configure (Scheduler.create [@alert "-experimental"]) ()
  let granularities ~f = List.iter [ 1; 3; 5; 12; 100 ] ~f

  module Test_sorts = struct
    let%expect_test "quickcheck" =
      Quickcheck.test
        [%generator: [%custom Quickcheck.Generator.small_positive_int] * int array]
        ~sexp_of:(fun (grain, array) -> [%message (grain : int) (array : int array)])
        ~f:(fun (grain, array) ->
          Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
            let array = Obj.magic_uncontended array in
            let expect = Base.Array.sorted_copy array ~compare in
            let observe =
              Arrays.Array.of_array array
              |> Arrays.Array.sort ~grain parallel ~compare:[%eta2 compare]
              |> Arrays.Array.to_array
            in
            let observe_stable =
              Arrays.Array.of_array array
              |> Arrays.Array.stable_sort ~grain parallel ~compare:[%eta2 compare]
              |> Arrays.Array.to_array
            in
            assert_eq observe ~expect;
            assert_eq observe_stable ~expect));
      [%expect {| |}]
    ;;

    let%expect_test "large" =
      let array = Array.init 1_000_000 ~f:(fun _ -> Random.int 1_000_000) in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Obj.magic_uncontended array in
        let expect = Array.sorted_copy array ~compare in
        let observe =
          Arrays.Array.of_array array
          |> Arrays.Array.sort ~grain:1024 parallel ~compare:[%eta2 compare]
          |> Arrays.Array.to_array
        in
        let observe_stable =
          Arrays.Array.of_array array
          |> Arrays.Array.stable_sort ~grain:1024 parallel ~compare:[%eta2 compare]
          |> Arrays.Array.to_array
        in
        assert_eq observe ~expect;
        assert_eq observe_stable ~expect);
      [%expect {| |}]
    ;;

    let%expect_test "stability" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array =
          Array.init 1_000_000 ~f:(fun _ -> Random.int 100, Random.int 1_000_000)
        in
        let compare = [%compare: int * _] in
        let observe : (int * int) array =
          Arrays.Array.of_array array
          |> Arrays.Array.stable_sort ~grain:3 parallel ~compare
          |> Arrays.Array.to_array
        in
        Array.stable_sort array ~compare:[%eta2 compare];
        assert_eq2 observe ~expect:array);
      [%expect {| |}]
    ;;
  end

  let%expect_test "fold_big" =
    Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
      let array = Array.init 100_000 ~f:Fn.id in
      granularities ~f:(fun grain ->
        Parallel.Arrays.Array.fold
          ~grain
          parallel
          (Arrays.Array.of_array array)
          ~init:0
          ~f:(fun acc i -> acc + i)
          ~combine:(fun a b -> a + b)
        |> printf "%d\n")
      [@nontail]);
    [%expect
      {|
      4999950000
      4999950000
      4999950000
      4999950000
      4999950000
      |}]
  ;;

  module Test_mut (Array : sig
    @@ portable
      type 'a t : mutable_data with 'a portable

      module Slice : Arrays.Slice with type 'a array := 'a t
      include Arrays.Inplace with type 'a t := 'a t

      val create : unit -> int t
      val print : int t -> unit
    end) =
  struct
    let%expect_test "slices" =
      let array = Array.create () in
      let s = Array.Slice.slice array in
      (* [get] *)
      printf "%d\n" (Array.Slice.get s 0);
      printf "%d\n" (Array.Slice.get s 9);
      [%expect
        {|
        0
        9
        |}];
      (* [set] and then [get] *)
      Array.Slice.set s 3 (-1);
      Array.Slice.set s 9 (-2);
      printf "%d\n" (Array.Slice.get s 3);
      printf "%d\n" (Array.Slice.get s 9);
      [%expect
        {|
        -1
        -2
        |}];
      (* [slice] bounds checks *)
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.slice array ~i:(-1) in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.slice array ~i:11 in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.slice array ~i:4 ~j:2 in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.slice array ~j:(-1) in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.slice array ~j:11 in
        ());
      [%expect
        {|
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        |}];
      (* [get] and [set] bounds checks *)
      require_raise (fun () ->
        let _ : int = Array.Slice.get s (-1) in
        ());
      require_raise (fun () ->
        let _ : int = Array.Slice.get s 10 in
        ());
      require_raise (fun () -> Array.Slice.set s (-1) 0);
      require_raise (fun () -> Array.Slice.set s 10 0);
      [%expect
        {|
        (Invalid_argument "index out of bounds")
        (Invalid_argument "index out of bounds")
        (Invalid_argument "index out of bounds")
        (Invalid_argument "index out of bounds")
        |}];
      (* [get] from [sub] *)
      let s' = Array.Slice.sub s ~i:2 ~j:5 in
      printf "%d\n" (Array.Slice.get s' 0);
      printf "%d\n" (Array.Slice.get s' 2);
      [%expect
        {|
        2
        4
        |}];
      (* [set] and then [get] from [sub] *)
      Array.Slice.set s 1 (-3);
      Array.Slice.set s 0 (-4);
      printf "%d\n" (Array.Slice.get s' 1);
      printf "%d\n" (Array.Slice.get s' 0);
      [%expect
        {|
        -1
        2
        |}];
      (* [sub] bounds checks *)
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.sub s ~i:(-1) in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.sub s ~i:11 in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.sub s ~i:4 ~j:2 in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.sub s ~j:(-1) in
        ());
      require_raise (fun () ->
        let _ : int Array.Slice.t = Array.Slice.sub s ~j:11 in
        ());
      [%expect
        {|
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        (Invalid_argument "invalid index range")
        |}]
    ;;

    [%%template
    [@@@mode.default m = (uncontended, shared)]

    let slice_to_list s =
      let n = Array.Slice.length s in
      let[@tail_mod_cons] rec aux i =
        if Base.Int.equal i n then [] else (Array.Slice.get [@mode m]) s i :: aux (i + 1)
      in
      aux 0 [@nontail]
    ;;

    let%expect_test "slice fork" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        let s = (Array.Slice.slice [@mode m]) array in
        let%template with_pivot pivot =
          (Array.Slice.fork_join2 [@mode m])
            parallel
            ?pivot
            s
            (fun _parallel s -> (slice_to_list [@mode m]) s)
            (fun _parallel s -> (slice_to_list [@mode m]) s)
        in
        let print_with_pivot ~pivot =
          let l0, l1 = with_pivot pivot in
          print_s [%message (pivot : int option) (l0 : int list) (l1 : int list)]
        in
        print_with_pivot ~pivot:None;
        print_with_pivot ~pivot:(Some 7);
        print_with_pivot ~pivot:(Some 0);
        print_with_pivot ~pivot:(Some 10);
        require_raise (fun () ->
          let _ : int list * int list = with_pivot (Some (-1)) in
          ());
        require_raise (fun () ->
          let _ : int list * int list = with_pivot (Some 11) in
          ())
        [@nontail]);
      [%expect
        {|
        ((pivot ()) (l0 (0 1 2 3 4)) (l1 (5 6 7 8 9)))
        ((pivot (7)) (l0 (0 1 2 3 4 5 6)) (l1 (7 8 9)))
        ((pivot (0)) (l0 ()) (l1 (0 1 2 3 4 5 6 7 8 9)))
        ((pivot (10)) (l0 (0 1 2 3 4 5 6 7 8 9)) (l1 ()))
        (Invalid_argument "index out of bounds")
        (Invalid_argument "index out of bounds")
        |}]
    ;;

    let%expect_test "empty slice fork" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        let s =
          (Array.Slice.slice [@mode m]) array |> (Array.Slice.sub [@mode m]) ~i:0 ~j:0
        in
        require_raise (fun () ->
          let (), () =
            (Array.Slice.fork_join2 [@mode m])
              parallel
              s
              (fun _parallel _s -> assert false)
              (fun _parallel _s -> assert false)
          in
          ())
        [@nontail]);
      [%expect {| (Invalid_argument "empty slice") |}]
    ;;]

    let%expect_test "map_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.map_inplace ~grain parallel array ~f:(fun i -> 2 * i);
          Array.print array);
        require_raise (fun () ->
          Array.map_inplace ~grain:0 parallel array ~f:(fun _ -> 0))
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 4 8 12 16 20 24 28 32 36))
        (array (0 8 16 24 32 40 48 56 64 72))
        (array (0 16 32 48 64 80 96 112 128 144))
        (array (0 32 64 96 128 160 192 224 256 288))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "mapi_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.mapi_inplace ~grain parallel array ~f:(fun idx i -> i + idx);
          Array.print array);
        require_raise (fun () ->
          Array.mapi_inplace ~grain:0 parallel array ~f:(fun _ _ -> 0))
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 3 6 9 12 15 18 21 24 27))
        (array (0 4 8 12 16 20 24 28 32 36))
        (array (0 5 10 15 20 25 30 35 40 45))
        (array (0 6 12 18 24 30 36 42 48 54))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "init_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.init_inplace ~grain parallel array ~f:(fun i -> 2 * i);
          Array.print array);
        require_raise (fun () ->
          Array.init_inplace ~grain:0 parallel array ~f:(fun _ -> 0))
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "sort_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.init_inplace parallel array ~f:(fun i -> Base.Int.(hash i % 100));
          Array.sort_inplace ~grain parallel array ~compare:[%eta2 compare];
          Array.print array);
        require_raise (fun () ->
          Array.sort_inplace ~grain:0 parallel array ~compare:[%eta2 compare])
        [@nontail]);
      [%expect
        {|
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "stable_sort_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.init_inplace parallel array ~f:(fun i -> Base.Int.(hash i % 100));
          Array.stable_sort_inplace ~grain parallel array ~compare:[%eta2 compare];
          Array.print array);
        require_raise (fun () ->
          Array.stable_sort_inplace ~grain:0 parallel array ~compare:[%eta2 compare])
        [@nontail]);
      [%expect
        {|
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (Invalid_argument "grain < 1")
        |}]
    ;;
  end

  module Test_immut (Array : sig
    @@ portable
      type 'a t : value mod portable with 'a portable

      include Arrays.Init with type 'a t := 'a t and type 'a init := int
      include Arrays.Map with type 'a t := 'a t
      include Arrays.Reduce with type 'a t := 'a t
      include Arrays.Sort with type 'a t := 'a t

      val create : unit -> int t
      val print : int t -> unit
    end) =
  struct
    let%expect_test "init" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        granularities ~f:(fun grain ->
          let array = Array.init ~grain parallel 10 ~f:(fun i -> 2 * i) in
          Array.print array);
        require_raise (fun () ->
          let _ : _ = Array.init ~grain:0 parallel 10 ~f:(fun _ -> 0) in
          ())
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "map" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let array = Array.map ~grain parallel array ~f:(fun i -> 2 * i) in
          Array.print array);
        require_raise (fun () ->
          let _ : _ = Array.map ~grain:0 parallel array ~f:(fun _ -> 0) in
          ())
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "mapi" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let array = Array.mapi ~grain parallel array ~f:(fun idx i -> i + idx) in
          Array.print array);
        require_raise (fun () ->
          let _ : _ = Array.mapi ~grain:0 parallel array ~f:(fun _ _ -> 0) in
          ())
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "map2" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let arraya = Array.create () in
        let arrayb = Array.create () in
        granularities ~f:(fun grain ->
          let array =
            Array.map2_exn ~grain parallel arraya arrayb ~f:(fun i j -> i + j)
          in
          Array.print array);
        require_raise (fun () ->
          let _ : _ = Array.map2_exn ~grain:0 parallel arraya arrayb ~f:(fun _ _ -> 0) in
          ())
        [@nontail]);
      [%expect
        {|
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (array (0 2 4 6 8 10 12 14 16 18))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "mapi2" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let arraya = Array.create () in
        let arrayb = Array.create () in
        granularities ~f:(fun grain ->
          let array =
            Array.mapi2_exn ~grain parallel arraya arrayb ~f:(fun idx i j -> i + j + idx)
          in
          Array.print array);
        require_raise (fun () ->
          let _ : _ =
            Array.mapi2_exn ~grain:0 parallel arraya arrayb ~f:(fun _ _ _ -> 0)
          in
          ())
        [@nontail]);
      [%expect
        {|
        (array (0 3 6 9 12 15 18 21 24 27))
        (array (0 3 6 9 12 15 18 21 24 27))
        (array (0 3 6 9 12 15 18 21 24 27))
        (array (0 3 6 9 12 15 18 21 24 27))
        (array (0 3 6 9 12 15 18 21 24 27))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "iter" =
      let sum = Atomic.make 0 in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.iter ~grain parallel array ~f:(fun i ->
            let _ : int = Atomic.fetch_and_add sum i in
            ()));
        require_raise (fun () -> Array.iter ~grain:0 parallel array ~f:(fun _ -> ()))
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect
        {|
        (Invalid_argument "grain < 1")
        ("Atomic.get sum" 225)
        |}]
    ;;

    let%expect_test "iteri" =
      let sum = Atomic.make 0 in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          Array.iteri ~grain parallel array ~f:(fun idx i ->
            let _ : int = Atomic.fetch_and_add sum (idx + i) in
            ()));
        require_raise (fun () -> Array.iteri ~grain:0 parallel array ~f:(fun _ _ -> ()))
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect
        {|
        (Invalid_argument "grain < 1")
        ("Atomic.get sum" 450)
        |}]
    ;;

    let swap_append l r =
      let[@tail_mod_cons] rec aux l r =
        match r with
        | [] -> l
        | r :: rr -> r :: aux l rr
      in
      aux l r
    ;;

    let%expect_test "fold" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let list =
            Array.fold
              ~grain
              parallel
              array
              ~init:[]
              ~f:(fun acc i -> i :: acc)
              ~combine:swap_append
          in
          print_s [%message (list : int list)]);
        require_raise (fun () ->
          Array.fold
            ~grain:0
            parallel
            array
            ~init:()
            ~f:(fun _ _ -> ())
            ~combine:(fun () () -> ()))
        [@nontail]);
      [%expect
        {|
        (list (9 8 7 6 5 4 3 2 1 0))
        (list (9 8 7 6 5 4 3 2 1 0))
        (list (9 8 7 6 5 4 3 2 1 0))
        (list (9 8 7 6 5 4 3 2 1 0))
        (list (9 8 7 6 5 4 3 2 1 0))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "foldi" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let list =
            Array.foldi
              ~grain
              parallel
              array
              ~init:[]
              ~f:(fun idx acc i -> (idx + i) :: acc)
              ~combine:swap_append
          in
          print_s [%message (list : int list)]);
        require_raise (fun () ->
          Array.foldi
            ~grain:0
            parallel
            array
            ~init:()
            ~f:(fun _ _ _ -> ())
            ~combine:(fun () () -> ()))
        [@nontail]);
      [%expect
        {|
        (list (18 16 14 12 10 8 6 4 2 0))
        (list (18 16 14 12 10 8 6 4 2 0))
        (list (18 16 14 12 10 8 6 4 2 0))
        (list (18 16 14 12 10 8 6 4 2 0))
        (list (18 16 14 12 10 8 6 4 2 0))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "reduce" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let option = Array.reduce ~grain parallel array ~f:(fun a b -> a + b) in
          print_s [%message (option : int option)]);
        require_raise (fun () ->
          let _ : _ = Array.reduce ~grain:0 parallel array ~f:(fun _ _ -> 0) in
          ())
        [@nontail]);
      [%expect
        {|
        (option (45))
        (option (45))
        (option (45))
        (option (45))
        (option (45))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "find" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let option = Array.find ~grain parallel array ~f:(fun a -> Base.Int.(a = 7)) in
          print_s [%message (option : int option)]);
        require_raise (fun () ->
          let _ : _ = Array.find ~grain:0 parallel array ~f:(fun _ -> false) in
          ())
        [@nontail]);
      [%expect
        {|
        (option (7))
        (option (7))
        (option (7))
        (option (7))
        (option (7))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "findi" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Array.create () in
        granularities ~f:(fun grain ->
          let option =
            Array.findi ~grain parallel array ~f:(fun idx _ -> Base.Int.(idx = 7))
          in
          print_s [%message (option : int option)]);
        require_raise (fun () ->
          let _ : _ = Array.findi ~grain:0 parallel array ~f:(fun _ _ -> false) in
          ())
        [@nontail]);
      [%expect
        {|
        (option (7))
        (option (7))
        (option (7))
        (option (7))
        (option (7))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "sort" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun i -> Base.Int.(hash i % 100)) in
        granularities ~f:(fun grain ->
          let array = Array.sort ~grain parallel array0 ~compare:[%eta2 compare] in
          Array.print array);
        require_raise (fun () ->
          let _ : _ = Array.sort ~grain:0 parallel array0 ~compare:[%eta2 compare] in
          ())
        [@nontail]);
      [%expect
        {|
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (Invalid_argument "grain < 1")
        |}]
    ;;

    let%expect_test "stable_sort" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun i -> Base.Int.(hash i % 100)) in
        granularities ~f:(fun grain ->
          let array = Array.stable_sort ~grain parallel array0 ~compare:[%eta2 compare] in
          Array.print array);
        require_raise (fun () ->
          let _ : _ =
            Array.stable_sort ~grain:0 parallel array0 ~compare:[%eta2 compare]
          in
          ())
        [@nontail]);
      [%expect
        {|
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (array (7 27 33 34 36 40 41 43 64 98))
        (Invalid_argument "grain < 1")
        |}]
    ;;
  end

  module Test_array = struct
    module Array = struct
      include Arrays.Array

      let create () = of_array ([| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |] : int array)

      let print array =
        let array = Array.to_list (to_array array) in
        print_s [%message (array : int list)]
      ;;
    end

    module _ = Test_mut (Array)
    module _ = Test_immut (Array)
  end

  module Test_iarray = struct
    module Array = struct
      include Arrays.Iarray

      let create () = of_iarray [: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 :]

      let print array =
        let array = Base.Iarray.to_list (to_iarray array) in
        print_s [%message (array : int list)]
      ;;
    end

    module _ = Test_immut (Array)
  end

  module Test_vec = struct
    module Array = struct
      include Arrays.Vec

      let create () = Vec.init 10 ~f:(fun i -> i) |> Obj.magic_portable |> of_vec

      let print array =
        let array = Vec.to_list (to_vec array) in
        print_s [%message (array : int list)]
      ;;
    end

    module _ = Test_mut (Array)
    module _ = Test_immut (Array)
  end

  module Test_bigstring = struct
    module Bigstring = Arrays.Bigstring
    open Bigstring.Kind

    let int_kinds
      ~(f :
          ('a : immutable_data).
          ('a t
          -> of_int:(int -> 'a) @ portable
          -> to_int:('a @ local -> int) @ portable
          -> unit)
          @ local)
      =
      f Int8 ~of_int:Int_repr.Int8.of_base_int_exn ~to_int:(fun a ->
        Int_repr.Int8.to_base_int a);
      f Int16 ~of_int:Int_repr.Int16.of_base_int_exn ~to_int:(fun a ->
        Int_repr.Int16.to_base_int a);
      f Int32 ~of_int:Int32.of_int_exn ~to_int:(fun a -> Int32.(to_int_exn (globalize a)));
      f Int64 ~of_int:Int64.of_int_exn ~to_int:(fun a -> Int64.(to_int_exn (globalize a)))
    ;;

    let%expect_test "init" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun i -> of_int (2 * i))
          in
          print_s [%message (bigstring : _ Bigstring.t)])
        [@nontail]);
      [%expect
        {|
        (bigstring ((kind Int8) (data  "\000\002\004\006\b\
                                      \n\012\014\016\018")))
        (bigstring
         ((kind Int16)
          (data
            "\000\000\002\000\004\000\006\000\b\000\
           \n\000\012\000\014\000\016\000\018\000")))
        (bigstring
         ((kind Int32)
          (data
            "\000\000\000\000\002\000\000\000\004\000\000\000\006\000\000\000\b\000\000\000\
           \n\000\000\000\012\000\000\000\014\000\000\000\016\000\000\000\018\000\000\000")))
        (bigstring
         ((kind Int64)
          (data
            "\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\
           \n\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000")))
        |}]
    ;;

    let%expect_test "map_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.create 8 in
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          Bigstring.map_inplace
            parallel
            (Bigstring.with_kind_exn kind bigstring)
            ~f:(fun _ -> of_int 1);
          print_s [%message (bigstring : bigstring)])
        [@nontail]);
      [%expect
        {|
        (bigstring "\001\001\001\001\001\001\001\001")
        (bigstring "\001\000\001\000\001\000\001\000")
        (bigstring "\001\000\000\000\001\000\000\000")
        (bigstring "\001\000\000\000\000\000\000\000")
        |}]
    ;;

    let%expect_test "map_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.create 8 in
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          Bigstring.mapi_inplace
            parallel
            (Bigstring.with_kind_exn kind bigstring)
            ~f:(fun idx _ -> of_int idx);
          print_s [%message (bigstring : bigstring)])
        [@nontail]);
      [%expect
        {|
        (bigstring "\000\001\002\003\004\005\006\007")
        (bigstring "\000\000\001\000\002\000\003\000")
        (bigstring "\000\000\000\000\001\000\000\000")
        (bigstring "\000\000\000\000\000\000\000\000")
        |}]
    ;;

    let%expect_test "iter" =
      let sum = Atomic.make 0 in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string "\000\001\002\003\004\005\006\007" in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          Bigstring.iter parallel (Bigstring.with_kind_exn kind bigstring) ~f:(fun i ->
            let _ : int = Atomic.fetch_and_add sum (to_int i) in
            ()))
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect {| ("Atomic.get sum" 506097523082532652) |}]
    ;;

    let%expect_test "iteri" =
      let sum = Atomic.make 0 in
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string "\000\001\002\003\004\005\006\007" in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          Bigstring.iteri
            parallel
            (Bigstring.with_kind_exn kind bigstring)
            ~f:(fun idx i ->
              let _ : int = Atomic.fetch_and_add sum (idx + to_int i) in
              ()))
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect {| ("Atomic.get sum" 506097523082532687) |}]
    ;;

    let[@tail_mod_cons] rec append l r =
      match l with
      | [] -> r
      | l :: ll -> l :: append ll r
    ;;

    let%expect_test "fold" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string "\000\001\002\003\004\005\006\007" in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          let list =
            Bigstring.fold
              parallel
              (Bigstring.with_kind_exn kind bigstring)
              ~init:[]
              ~f:(fun acc i -> i :: acc)
              ~combine:append
            |> List.map ~f:(fun i -> to_int i)
          in
          print_s [%message (list : int list)])
        [@nontail]);
      [%expect
        {|
        (list (7 6 5 4 3 2 1 0))
        (list (1798 1284 770 256))
        (list (117835012 50462976))
        (list (506097522914230528))
        |}]
    ;;

    let%expect_test "reduce" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string "\000\001\002\003\004\005\006\007" in
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let option =
            Bigstring.reduce
              parallel
              (Bigstring.with_kind_exn kind bigstring)
              ~f:(fun a b -> to_int a + to_int b |> of_int)
            |> Option.map ~f:(fun i -> to_int i)
          in
          print_s [%message (option : int option)])
        [@nontail]);
      [%expect
        {|
        (option (28))
        (option (4108))
        (option (168297988))
        (option (506097522914230528))
        |}]
    ;;

    let%expect_test "find" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string "\000\001\002\003\004\005\006\007" in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          let option =
            Bigstring.find parallel (Bigstring.with_kind_exn kind bigstring) ~f:(fun a ->
              let a = to_int a in
              Base.Int.(a % 4 = 0))
            |> Option.map ~f:(fun i -> to_int i)
          in
          print_s [%message (option : int option)])
        [@nontail]);
      [%expect
        {|
        (option (0))
        (option (256))
        (option (50462976))
        (option (506097522914230528))
        |}]
    ;;

    let dump_bytes bigstring = print_s [%message (bigstring : Bytes.Hexdump.t)]

    let dump : type a. a Arrays.Bigstring.t -> unit =
      fun bigstring -> Base_bigstring.to_bytes bigstring.data |> dump_bytes
    ;;

    let%expect_test "sort_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun i ->
              Base.Int.(hash i % 100) |> of_int)
          in
          Bigstring.sort_inplace parallel bigstring ~compare:(fun a b ->
            compare (to_int a) (to_int b));
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  07 1b 21 22 24 28 29 2b  40 62                    |..!\"$()+@b|"))
        (bigstring
         ("00000000  07 00 1b 00 21 00 22 00  24 00 28 00 29 00 2b 00  |....!.\".$.(.).+.|"
          "00000010  40 00 62 00                                       |@.b.|"))
        (bigstring
         ("00000000  07 00 00 00 1b 00 00 00  21 00 00 00 22 00 00 00  |........!...\"...|"
          "00000010  24 00 00 00 28 00 00 00  29 00 00 00 2b 00 00 00  |$...(...)...+...|"
          "00000020  40 00 00 00 62 00 00 00                           |@...b...|"))
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |................|"
          "00000010  21 00 00 00 00 00 00 00  22 00 00 00 00 00 00 00  |!.......\".......|"
          "00000020  24 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  |$.......(.......|"
          "00000030  29 00 00 00 00 00 00 00  2b 00 00 00 00 00 00 00  |).......+.......|"
          "00000040  40 00 00 00 00 00 00 00  62 00 00 00 00 00 00 00  |@.......b.......|"))
        |}]
    ;;

    let%expect_test "sort" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun i ->
              Base.Int.(hash i % 100) |> of_int)
          in
          let bigstring =
            Bigstring.sort parallel bigstring ~compare:(fun a b ->
              compare (to_int a) (to_int b))
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  07 1b 21 22 24 28 29 2b  40 62                    |..!\"$()+@b|"))
        (bigstring
         ("00000000  07 00 1b 00 21 00 22 00  24 00 28 00 29 00 2b 00  |....!.\".$.(.).+.|"
          "00000010  40 00 62 00                                       |@.b.|"))
        (bigstring
         ("00000000  07 00 00 00 1b 00 00 00  21 00 00 00 22 00 00 00  |........!...\"...|"
          "00000010  24 00 00 00 28 00 00 00  29 00 00 00 2b 00 00 00  |$...(...)...+...|"
          "00000020  40 00 00 00 62 00 00 00                           |@...b...|"))
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |................|"
          "00000010  21 00 00 00 00 00 00 00  22 00 00 00 00 00 00 00  |!.......\".......|"
          "00000020  24 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  |$.......(.......|"
          "00000030  29 00 00 00 00 00 00 00  2b 00 00 00 00 00 00 00  |).......+.......|"
          "00000040  40 00 00 00 00 00 00 00  62 00 00 00 00 00 00 00  |@.......b.......|"))
        |}]
    ;;

    let%expect_test "stable_sort_inplace" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun i ->
              Base.Int.(hash i % 100) |> of_int)
          in
          Bigstring.stable_sort_inplace parallel bigstring ~compare:(fun a b ->
            compare (to_int a) (to_int b));
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  07 1b 21 22 24 28 29 2b  40 62                    |..!\"$()+@b|"))
        (bigstring
         ("00000000  07 00 1b 00 21 00 22 00  24 00 28 00 29 00 2b 00  |....!.\".$.(.).+.|"
          "00000010  40 00 62 00                                       |@.b.|"))
        (bigstring
         ("00000000  07 00 00 00 1b 00 00 00  21 00 00 00 22 00 00 00  |........!...\"...|"
          "00000010  24 00 00 00 28 00 00 00  29 00 00 00 2b 00 00 00  |$...(...)...+...|"
          "00000020  40 00 00 00 62 00 00 00                           |@...b...|"))
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |................|"
          "00000010  21 00 00 00 00 00 00 00  22 00 00 00 00 00 00 00  |!.......\".......|"
          "00000020  24 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  |$.......(.......|"
          "00000030  29 00 00 00 00 00 00 00  2b 00 00 00 00 00 00 00  |).......+.......|"
          "00000040  40 00 00 00 00 00 00 00  62 00 00 00 00 00 00 00  |@.......b.......|"))
        |}]
    ;;

    let%expect_test "stable_sort" =
      Scheduler.schedule scheduler ~monitor ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun i ->
              Base.Int.(hash i % 100) |> of_int)
          in
          let bigstring =
            Bigstring.stable_sort parallel bigstring ~compare:(fun a b ->
              compare (to_int a) (to_int b))
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  07 1b 21 22 24 28 29 2b  40 62                    |..!\"$()+@b|"))
        (bigstring
         ("00000000  07 00 1b 00 21 00 22 00  24 00 28 00 29 00 2b 00  |....!.\".$.(.).+.|"
          "00000010  40 00 62 00                                       |@.b.|"))
        (bigstring
         ("00000000  07 00 00 00 1b 00 00 00  21 00 00 00 22 00 00 00  |........!...\"...|"
          "00000010  24 00 00 00 28 00 00 00  29 00 00 00 2b 00 00 00  |$...(...)...+...|"
          "00000020  40 00 00 00 62 00 00 00                           |@...b...|"))
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |................|"
          "00000010  21 00 00 00 00 00 00 00  22 00 00 00 00 00 00 00  |!.......\".......|"
          "00000020  24 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  |$.......(.......|"
          "00000030  29 00 00 00 00 00 00 00  2b 00 00 00 00 00 00 00  |).......+.......|"
          "00000040  40 00 00 00 00 00 00 00  62 00 00 00 00 00 00 00  |@.......b.......|"))
        |}]
    ;;
  end
end

include Common.Test_schedulers (Test_scheduler)
