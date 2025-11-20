open! Core
open! Import
module Arrays = Parallel.Arrays

let require_raise = Expect_test_helpers_core.require_does_raise
let assert_eq = [%test_result: int array]
let assert_eq2 = [%test_result: (int * int) array]

let swap_append _ l r =
  let[@tail_mod_cons] rec aux l r =
    match r with
    | [] -> l
    | r :: rr -> r :: aux l rr
  in
  aux l r
;;

module Test_scheduler (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ()

  module%test Test_sorts = struct
    let%expect_test "quickcheck" =
      Quickcheck.test
        [%generator: int array]
        ~sexp_of:(fun array -> [%message (array : int array)])
        ~f:(fun array ->
          Scheduler.parallel scheduler ~f:(fun parallel ->
            let array = Obj.magic_uncontended array in
            let expect = Base.Array.sorted_copy array ~compare in
            let observe =
              Arrays.Array.of_array array
              |> Arrays.Array.sort parallel ~compare:(fun _ -> [%eta2 compare])
              |> Arrays.Array.to_array
            in
            let observe_stable =
              Arrays.Array.of_array array
              |> Arrays.Array.stable_sort parallel ~compare:(fun _ -> [%eta2 compare])
              |> Arrays.Array.to_array
            in
            assert_eq observe ~expect;
            assert_eq observe_stable ~expect));
      [%expect {| |}]
    ;;

    let%expect_test "large" =
      let array = Array.init 1_000_000 ~f:(fun _ -> Random.int 1_000_000) in
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Obj.magic_uncontended array in
        let expect = Array.sorted_copy array ~compare in
        let observe =
          Arrays.Array.of_array array
          |> Arrays.Array.sort parallel ~compare:(fun _ -> [%eta2 compare])
          |> Arrays.Array.to_array
        in
        let observe_stable =
          Arrays.Array.of_array array
          |> Arrays.Array.stable_sort parallel ~compare:(fun _ -> [%eta2 compare])
          |> Arrays.Array.to_array
        in
        assert_eq observe ~expect;
        assert_eq observe_stable ~expect);
      [%expect {| |}]
    ;;

    let%expect_test "stability" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array =
          Array.init 1_000_000 ~f:(fun _ -> Random.int 100, Random.int 1_000_000)
        in
        let compare = [%compare: int * _] in
        let observe : (int * int) array =
          Arrays.Array.of_array array
          |> Arrays.Array.stable_sort parallel ~compare:(fun _ -> [%eta2 compare])
          |> Arrays.Array.to_array
        in
        Array.stable_sort array ~compare:[%eta2 compare];
        assert_eq2 observe ~expect:array);
      [%expect {| |}]
    ;;
  end

  let%expect_test "fold_big" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let array = Array.init 100_000 ~f:Fn.id in
      Parallel.Arrays.Array.fold
        parallel
        (Arrays.Array.of_array array)
        ~init:(fun () -> 0)
        ~f:(fun _ acc i -> acc + i)
        ~combine:(fun _ a b -> a + b)
      |> printf "%d\n");
    [%expect {| 4999950000 |}]
  ;;

  let%expect_test "scan big" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let array = Array.init 1_000_000 ~f:(fun _ -> Random.int 1_000_000) in
      let expect_result, expect_scanned =
        Array.fold_map array ~init:0 ~f:(fun acc a -> acc + a, acc)
      in
      let scanned, result =
        Parallel.Arrays.Array.scan
          parallel
          (Arrays.Array.of_array array)
          ~init:0
          ~f:(fun _ a b -> a + b)
      in
      assert_eq (Arrays.Array.to_array scanned) ~expect:expect_scanned;
      [%test_result: int] result ~expect:expect_result);
    [%expect {| |}]
  ;;

  let%expect_test "scan_inclusive big" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let array = Array.init 1_000_000 ~f:(fun _ -> Random.int 1_000_000) in
      let _, expect_scanned =
        Array.fold_map array ~init:0 ~f:(fun acc a -> acc + a, acc + a)
      in
      let scanned =
        Parallel.Arrays.Array.scan_inclusive
          parallel
          (Arrays.Array.of_array array)
          ~init:0
          ~f:(fun _ a b -> a + b)
      in
      assert_eq (Arrays.Array.to_array scanned) ~expect:expect_scanned);
    [%expect {| |}]
  ;;

  let%expect_test "filter big" =
    Scheduler.parallel scheduler ~f:(fun parallel ->
      let array = Array.init 1_000_000 ~f:(fun _ -> Random.int 1_000_000) in
      let expect = Array.filter array ~f:(fun i -> i >= 500_000) in
      let filtered =
        Parallel.Arrays.Array.filter parallel (Arrays.Array.of_array array) ~f:(fun _ i ->
          i >= 500_000)
      in
      assert_eq (Arrays.Array.to_array filtered) ~expect);
    [%expect {| |}]
  ;;

  module Test_mut (Array : sig
    @@ portable
      type ('a : value mod portable) t : value mod portable

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
      Scheduler.parallel scheduler ~f:(fun parallel ->
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
          let #(l0, l1) = with_pivot pivot in
          print_s [%message (pivot : int option) (l0 : int list) (l1 : int list)]
        in
        print_with_pivot ~pivot:None;
        print_with_pivot ~pivot:(Some 7);
        print_with_pivot ~pivot:(Some 0);
        print_with_pivot ~pivot:(Some 10);
        require_raise (fun () ->
          let _ : #(int list * int list) = with_pivot (Some (-1)) in
          ());
        require_raise (fun () ->
          let _ : #(int list * int list) = with_pivot (Some 11) in
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

    let%expect_test "empty slice" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let s =
          (Array.Slice.slice [@mode m]) array |> (Array.Slice.sub [@mode m]) ~i:0 ~j:0
        in
        let len label s =
          let length = Array.Slice.length s in
          print_s [%message label (length : int)]
        in
        let #((), ()) =
          (Array.Slice.fork_join2 [@mode m])
            parallel
            s
            (fun _parallel s -> len "fork_join" s)
            (fun _parallel s -> len "fork_join" s)
        in
        Array.Slice.(for_ [@mode m]) parallel ~pivots:[::] s ~f:(fun _ s ->
          len "for_ without pivots" s);
        Array.Slice.(for_ [@mode m]) parallel ~pivots:[: 0; 0; 0 :] s ~f:(fun _ s ->
          len "for_ with pivots" s)
        [@nontail]);
      [%expect
        {|
        (fork_join (length 0))
        (fork_join (length 0))
        ("for_ without pivots" (length 0))
        ("for_ with pivots" (length 0))
        ("for_ with pivots" (length 0))
        ("for_ with pivots" (length 0))
        ("for_ with pivots" (length 0))
        |}]
    ;;]

    let%expect_test "parallel for over slices" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.print array;
        let slice = Array.Slice.slice array in
        let len = Array.Slice.length slice in
        let pivots = Iarray.init (len - 1) ~f:(fun i -> i + 1) in
        Array.Slice.for_ parallel ~pivots slice ~f:(fun _ slice ->
          let x = Array.Slice.get slice 0 in
          Array.Slice.set slice 0 (x + 1));
        Array.print array);
      [%expect
        {|
        (array (0 1 2 3 4 5 6 7 8 9))
        (array (1 2 3 4 5 6 7 8 9 10))
        |}]
    ;;

    let%expect_test "mixing slice.sub/fork_join2 with for_" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.print array;
        let slice = Array.Slice.slice array in
        let subslice = Array.Slice.sub slice ~i:1 ~j:9 in
        let pivots = [: 2; 4; 6 :] in
        Array.Slice.for_ parallel ~pivots subslice ~f:(fun _ slice ->
          for i = 0 to Array.Slice.length slice - 1 do
            let x = Array.Slice.get slice i in
            Array.Slice.set slice i (x * 2)
          done);
        Array.print array;
        let #((), ()) =
          Array.Slice.fork_join2
            parallel
            ~pivot:5
            slice
            (fun parallel left_slice ->
              let pivots = [: 1; 3 :] in
              Array.Slice.for_ parallel ~pivots left_slice ~f:(fun _ s ->
                for i = 0 to Array.Slice.length s - 1 do
                  let x = Array.Slice.get s i in
                  Array.Slice.set s i (x + 10)
                done))
            (fun parallel right_slice ->
              let pivots = [: 2; 4 :] in
              Array.Slice.for_ parallel ~pivots right_slice ~f:(fun _ s ->
                for i = 0 to Array.Slice.length s - 1 do
                  let x = Array.Slice.get s i in
                  Array.Slice.set s i (x + 100)
                done))
        in
        Array.print array [@nontail]);
      [%expect
        {|
        (array (0 1 2 3 4 5 6 7 8 9))
        (array (0 2 4 6 8 10 12 14 16 9))
        (array (10 12 14 16 18 110 112 114 116 109))
        |}]
    ;;

    let%expect_test "parallel fori over slices" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.print array;
        let slice = Array.Slice.slice array in
        let len = Array.Slice.length slice in
        let pivots = Iarray.init (len - 1) ~f:(fun i -> i + 1) in
        Array.Slice.fori parallel ~pivots slice ~f:(fun _ i slice ->
          let x = Array.Slice.get slice 0 in
          Array.Slice.set slice 0 (x + i));
        Array.print array);
      [%expect
        {|
        (array (0 1 2 3 4 5 6 7 8 9))
        (array (0 2 4 6 8 10 12 14 16 18))
        |}]
    ;;

    let%expect_test "bad pivots inputs" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let slice = Array.Slice.slice array in
        let len = Array.Slice.length slice in
        let pivots = [: 1; len + 1 :] in
        require_raise (fun () ->
          Array.Slice.for_ parallel ~pivots slice ~f:(fun _ _ -> ()));
        let pivots = [: -1; 1 :] in
        require_raise (fun () ->
          Array.Slice.for_ parallel ~pivots slice ~f:(fun _ _ -> ()));
        let pivots = [: 3; 1 :] in
        require_raise (fun () ->
          Array.Slice.for_ parallel ~pivots slice ~f:(fun _ _ -> ()))
        [@nontail]);
      [%expect
        {|
        (Invalid_argument "index out of bounds")
        (Invalid_argument "index out of bounds")
        (Invalid_argument "pivots must be non-decreasing")
        |}]
    ;;

    let%expect_test "map_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.map_inplace parallel array ~f:(fun _ i -> 2 * i);
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "mapi_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.mapi_inplace parallel array ~f:(fun _ idx i -> i + idx);
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "init_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.init_inplace parallel array ~f:(fun _ i -> 2 * i);
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "sort_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.init_inplace parallel array ~f:(fun _ i -> Base.Int.(hash i % 100));
        Array.sort_inplace parallel array ~compare:(fun _ -> [%eta2 compare]);
        Array.print array [@nontail]);
      [%expect {| (array (7 27 33 34 36 40 41 43 64 98)) |}]
    ;;

    let%expect_test "stable_sort_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.init_inplace parallel array ~f:(fun _ i -> Base.Int.(hash i % 100));
        Array.stable_sort_inplace parallel array ~compare:(fun _ -> [%eta2 compare]);
        Array.print array [@nontail]);
      [%expect {| (array (7 27 33 34 36 40 41 43 64 98)) |}]
    ;;

    let%expect_test "scan_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.init_inplace parallel array ~f:(fun _ i -> Base.Int.(hash i % 100));
        let total = Array.scan_inplace parallel array ~init:0 ~f:(fun _ a b -> a + b) in
        Array.print array;
        print_s [%sexp ~~(total : int)] [@nontail]);
      [%expect
        {|
        (array (0 64 98 138 179 212 248 346 353 396))
        (total 423)
        |}]
    ;;

    let%expect_test "scan_inclusive_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.init_inplace parallel array ~f:(fun _ i -> Base.Int.(hash i % 100));
        Array.scan_inclusive_inplace parallel array ~init:0 ~f:(fun _ a b -> a + b);
        Array.print array [@nontail]);
      [%expect {| (array (64 98 138 179 212 248 346 353 396 423)) |}]
    ;;
  end

  module Test_immut (Array : sig
    @@ portable
      type ('a : value mod portable) t : value mod portable

      include Arrays.Init with type 'a t := 'a t and type 'a init := int
      include Arrays.Map with type 'a t := 'a t
      include Arrays.Reduce with type 'a t := 'a t
      include Arrays.Sort with type 'a t := 'a t
      include Arrays.Scan with type 'a t := 'a t
      include Arrays.Filter with type 'a t := 'a t

      val create : unit -> int t
      val print : int t -> unit
    end) =
  struct
    let%expect_test "init" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.init parallel 10 ~f:(fun _ i -> 2 * i) in
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let array = Array.map parallel array ~f:(fun _ i -> 2 * i) in
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "mapi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let array = Array.mapi parallel array ~f:(fun _ idx i -> i + idx) in
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "map2" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let arraya = Array.create () in
        let arrayb = Array.create () in
        let array = Array.map2_exn parallel arraya arrayb ~f:(fun _ i j -> i + j) in
        Array.print array [@nontail]);
      [%expect {| (array (0 2 4 6 8 10 12 14 16 18)) |}]
    ;;

    let%expect_test "mapi2" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let arraya = Array.create () in
        let arrayb = Array.create () in
        let array =
          Array.mapi2_exn parallel arraya arrayb ~f:(fun _ idx i j -> i + j + idx)
        in
        Array.print array [@nontail]);
      [%expect {| (array (0 3 6 9 12 15 18 21 24 27)) |}]
    ;;

    let%expect_test "iter" =
      let sum = Atomic.make 0 in
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.iter parallel array ~f:(fun _ i ->
          let _ : int = Atomic.fetch_and_add sum i in
          ())
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect {| ("Atomic.get sum" 45) |}]
    ;;

    let%expect_test "iteri" =
      let sum = Atomic.make 0 in
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        Array.iteri parallel array ~f:(fun _ idx i ->
          let _ : int = Atomic.fetch_and_add sum (idx + i) in
          ())
        [@nontail]);
      print_s [%message (Atomic.get sum : int)];
      [%expect {| ("Atomic.get sum" 90) |}]
    ;;

    let%expect_test "fold" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let list =
          Array.fold
            parallel
            array
            ~init:(fun () : int list -> [])
            ~f:(fun _ acc i -> i :: acc)
            ~combine:swap_append
        in
        print_s [%message (list : int list)] [@nontail]);
      [%expect {| (list (9 8 7 6 5 4 3 2 1 0)) |}]
    ;;

    let%expect_test "foldi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let list =
          Array.foldi
            parallel
            array
            ~init:(fun () : int list -> [])
            ~f:(fun _ idx acc i -> (idx + i) :: acc)
            ~combine:swap_append
        in
        print_s [%message (list : int list)] [@nontail]);
      [%expect {| (list (18 16 14 12 10 8 6 4 2 0)) |}]
    ;;

    let%expect_test "reduce" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let option = Array.reduce parallel array ~f:(fun _ a b -> a + b) in
        print_s [%message (option : int option)] [@nontail]);
      [%expect {| (option (45)) |}]
    ;;

    let%expect_test "min_elt" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let option =
          Array.min_elt parallel array ~compare:(fun _par -> [%compare: int])
        in
        print_s [%message (option : int option)] [@nontail]);
      [%expect {| (option (0)) |}]
    ;;

    let%expect_test "max_elt" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let option =
          Array.max_elt parallel array ~compare:(fun _par -> [%compare: int])
        in
        print_s [%message (option : int option)] [@nontail]);
      [%expect {| (option (9)) |}]
    ;;

    let%expect_test "find" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let option = Array.find parallel array ~f:(fun _ a -> Base.Int.(a = 7)) in
        print_s [%message (option : int option)] [@nontail]);
      [%expect {| (option (7)) |}]
    ;;

    let%expect_test "findi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array = Array.create () in
        let option = Array.findi parallel array ~f:(fun _ idx _ -> Base.Int.(idx = 7)) in
        print_s [%message (option : int option)] [@nontail]);
      [%expect {| (option (7)) |}]
    ;;

    let%expect_test "sort" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array = Array.sort parallel array0 ~compare:(fun _ -> [%eta2 compare]) in
        Array.print array [@nontail]);
      [%expect {| (array (7 27 33 34 36 40 41 43 64 98)) |}]
    ;;

    let%expect_test "stable_sort" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.stable_sort parallel array0 ~compare:(fun _ -> [%eta2 compare])
        in
        Array.print array [@nontail]);
      [%expect {| (array (7 27 33 34 36 40 41 43 64 98)) |}]
    ;;

    let%expect_test "scan" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array, total = Array.scan parallel array0 ~init:0 ~f:(fun _ a b -> a + b) in
        Array.print array;
        print_s [%sexp ~~(total : int)] [@nontail]);
      [%expect
        {|
        (array (0 64 98 138 179 212 248 346 353 396))
        (total 423)
        |}]
    ;;

    let%expect_test "scan_inclusive" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.scan_inclusive parallel array0 ~init:0 ~f:(fun _ a b -> a + b)
        in
        Array.print array [@nontail]);
      [%expect {| (array (64 98 138 179 212 248 346 353 396 423)) |}]
    ;;

    let%expect_test "filter" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array = Array.filter parallel array0 ~f:(fun _ a -> a < 50) in
        Array.print array [@nontail]);
      [%expect {| (array (34 40 41 33 36 7 43 27)) |}]
    ;;

    let%expect_test "filteri" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array = Array.filteri parallel array0 ~f:(fun _ i a -> i >= 5 && a < 50) in
        Array.print array [@nontail]);
      [%expect {| (array (36 7 43 27)) |}]
    ;;
  end

  module%test Test_array = struct
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

    let%expect_test "filter_map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.filter_map parallel array0 ~f:(fun _ a -> if a < 50 then This a else Null)
        in
        Array.print array [@nontail]);
      [%expect {| (array (34 40 41 33 36 7 43 27)) |}]
    ;;

    let%expect_test "filter_mapi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.filter_mapi parallel array0 ~f:(fun _ i a ->
            if i >= 5 && a < 50 then This a else Null)
        in
        Array.print array [@nontail]);
      [%expect {| (array (36 7 43 27)) |}]
    ;;
  end

  module%test Test_iarray = struct
    module Array = struct
      include Arrays.Iarray

      let create () = of_iarray [: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 :]

      let print array =
        let array = Base.Iarray.to_list (to_iarray array) in
        print_s [%message (array : int list)]
      ;;
    end

    module _ = Test_immut (Array)

    let%expect_test "filter_map" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.filter_map parallel array0 ~f:(fun _ a -> if a < 50 then This a else Null)
        in
        Array.print array [@nontail]);
      [%expect {| (array (34 40 41 33 36 7 43 27)) |}]
    ;;

    let%expect_test "filter_mapi" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let array0 = Array.init parallel 10 ~f:(fun _ i -> Base.Int.(hash i % 100)) in
        let array =
          Array.filter_mapi parallel array0 ~f:(fun _ i a ->
            if i >= 5 && a < 50 then This a else Null)
        in
        Array.print array [@nontail]);
      [%expect {| (array (36 7 43 27)) |}]
    ;;
  end

  module%test Test_vec = struct
    module Array = struct
      include Arrays.Vec

      let create () = of_vec (Vec.init 10 ~f:(fun i -> i))

      let print array =
        let array = Vec.to_list (to_vec array) in
        print_s [%message (array : int list)]
      ;;
    end

    module _ = Test_mut (Array)
    module _ = Test_immut (Array)
  end

  module%test Test_bigstring = struct
    module Bigstring = Arrays.Bigstring
    open Bigstring.Kind

    let string_0_31 = String.init 32 ~f:Char.of_int_exn

    let int_kinds
      ~(f :
          ('a : immutable_data).
          ('a t
          -> of_int:(Bigint.t -> 'a) @ portable
          -> to_int:('a @ local -> Bigint.t) @ portable
          -> unit)
          @ local)
      =
      let open Ocaml_simd_sse in
      let bigint_to_int64_trunc i = Bigint.(to_int64_exn (i land ((one lsl 64) - one))) in
      f
        Int8
        ~of_int:(fun i -> Int8.of_int Bigint.(to_int_exn (i land ((one lsl 8) - one))))
        ~to_int:(fun a -> Bigint.of_int (Int8.to_int a));
      f
        Int16
        ~of_int:(fun i -> Int16.of_int Bigint.(to_int_exn (i land ((one lsl 16) - one))))
        ~to_int:(fun a -> Bigint.of_int (Int16.to_int a));
      f
        Int32
        ~of_int:(fun i ->
          Int32.of_int64_trunc Bigint.(to_int64_exn (i land ((one lsl 32) - one))))
        ~to_int:(fun a -> Bigint.of_int32_exn (Int32.globalize a));
      f Int64 ~of_int:bigint_to_int64_trunc ~to_int:(fun i ->
        Bigint.of_int64_exn (Int64.globalize i));
      f
        Int64x2
        ~of_int:(fun i ->
          let i0 = Int64_u.of_int64 (bigint_to_int64_trunc i) in
          let i1 = Int64_u.of_int64 (bigint_to_int64_trunc Bigint.(i asr 64)) in
          Int64x2.set i0 i1 |> Int64x2.box)
        ~to_int:(fun a ->
          let #(i0, i1) = Int64x2.splat (Int64x2.unbox a) in
          Bigint.(
            (of_int64_exn (Int64_u.to_int64 i1) lsl 64)
            + of_int64_exn (Int64_u.to_int64 i0)))
    ;;

    let dump_bytes bigstring = print_s [%message (bigstring : Bytes.Hexdump.t)]

    let dump : type a. a Arrays.Bigstring.t -> unit =
      fun bigstring -> Base_bigstring.to_bytes bigstring.data |> dump_bytes
    ;;

    let%expect_test "init" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          let bigstring =
            Bigstring.init parallel (kind, 4) ~f:(fun _ i ->
              of_int (Bigint.of_int (2 * i)))
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  00 02 04 06                                       |....|"))
        (bigstring
         ("00000000  00 00 02 00 04 00 06 00                           |........|"))
        (bigstring
         ("00000000  00 00 00 00 02 00 00 00  04 00 00 00 06 00 00 00  |................|"))
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  02 00 00 00 00 00 00 00  |................|"
          "00000010  04 00 00 00 00 00 00 00  06 00 00 00 00 00 00 00  |................|"))
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  02 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  04 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000030  06 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"))
        |}]
    ;;

    let%expect_test "map_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          let bigstring = Bigstring.with_kind_exn kind (Base_bigstring.create 32) in
          Bigstring.map_inplace parallel bigstring ~f:(fun _ _ ->
            of_int (Bigint.of_int 1));
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  01 01 01 01 01 01 01 01  01 01 01 01 01 01 01 01  |................|"
          "00000010  01 01 01 01 01 01 01 01  01 01 01 01 01 01 01 01  |................|"))
        (bigstring
         ("00000000  01 00 01 00 01 00 01 00  01 00 01 00 01 00 01 00  |................|"
          "00000010  01 00 01 00 01 00 01 00  01 00 01 00 01 00 01 00  |................|"))
        (bigstring
         ("00000000  01 00 00 00 01 00 00 00  01 00 00 00 01 00 00 00  |................|"
          "00000010  01 00 00 00 01 00 00 00  01 00 00 00 01 00 00 00  |................|"))
        (bigstring
         ("00000000  01 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00  |................|"
          "00000010  01 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00  |................|"))
        (bigstring
         ("00000000  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"))
        |}]
    ;;

    let%expect_test "map_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int:_ ->
          let bigstring = Bigstring.with_kind_exn kind (Base_bigstring.create 32) in
          Bigstring.mapi_inplace parallel bigstring ~f:(fun _ idx _ ->
            of_int (Bigint.of_int idx));
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  |................|"
          "00000010  10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f  |................|"))
        (bigstring
         ("00000000  00 00 01 00 02 00 03 00  04 00 05 00 06 00 07 00  |................|"
          "00000010  08 00 09 00 0a 00 0b 00  0c 00 0d 00 0e 00 0f 00  |................|"))
        (bigstring
         ("00000000  00 00 00 00 01 00 00 00  02 00 00 00 03 00 00 00  |................|"
          "00000010  04 00 00 00 05 00 00 00  06 00 00 00 07 00 00 00  |................|"))
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00  |................|"
          "00000010  02 00 00 00 00 00 00 00  03 00 00 00 00 00 00 00  |................|"))
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"))
        |}]
    ;;

    let%expect_test "iter" =
      let sum = Atomic.make Bigint.zero in
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string string_0_31 in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          Bigstring.iter parallel (Bigstring.with_kind_exn kind bigstring) ~f:(fun _ i ->
            Atomic.update sum ~pure_f:(fun sum -> Bigint.( + ) sum (to_int i))))
        [@nontail]);
      print_s [%message (Atomic.get sum : Bigint.t)];
      [%expect {| ("Atomic.get sum" 61373803910015629371101599527580058000) |}]
    ;;

    let%expect_test "iteri" =
      let sum = Atomic.make Bigint.zero in
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string string_0_31 in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          Bigstring.iteri
            parallel
            (Bigstring.with_kind_exn kind bigstring)
            ~f:(fun _ idx i ->
              let i = Bigint.( + ) (Bigint.of_int idx) (to_int i) in
              Atomic.update sum ~pure_f:(fun sum -> Bigint.(sum + i))))
        [@nontail]);
      print_s [%message (Atomic.get sum : Bigint.t)];
      [%expect {| ("Atomic.get sum" 61373803910015629371101599527580058651) |}]
    ;;

    let%expect_test "fold" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string string_0_31 in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          let list =
            Bigstring.fold
              parallel
              (Bigstring.with_kind_exn kind bigstring)
              ~init:(fun () : (_ : value mod portable) list -> [])
              ~f:(fun _ acc i -> i :: acc)
              ~combine:swap_append
            |> List.map ~f:(fun i -> to_int i)
          in
          print_s [%message (list : Bigint.Hex.t list)])
        [@nontail]);
      [%expect
        {|
        (list
         (0x1f 0x1e 0x1d 0x1c 0x1b 0x1a 0x19 0x18 0x17 0x16 0x15 0x14 0x13 0x12 0x11
          0x10 0xf 0xe 0xd 0xc 0xb 0xa 0x9 0x8 0x7 0x6 0x5 0x4 0x3 0x2 0x1 0x0))
        (list
         (0x1f1e 0x1d1c 0x1b1a 0x1918 0x1716 0x1514 0x1312 0x1110 0xf0e 0xd0c 0xb0a
          0x908 0x706 0x504 0x302 0x100))
        (list
         (0x1f1e1d1c 0x1b1a1918 0x17161514 0x13121110 0xf0e0d0c 0xb0a0908 0x7060504
          0x3020100))
        (list
         (0x1f1e1d1c1b1a1918 0x1716151413121110 0xf0e0d0c0b0a0908 0x706050403020100))
        (list (0x1f1e1d1c1b1a19181716151413121110 0xf0e0d0c0b0a09080706050403020100))
        |}]
    ;;

    let%expect_test "reduce" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string string_0_31 in
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let option =
            Bigstring.reduce
              parallel
              (Bigstring.with_kind_exn kind bigstring)
              ~f:(fun _ a b -> Bigint.( + ) (to_int a) (to_int b) |> of_int)
            |> Option.map ~f:(fun i -> to_int i)
          in
          print_s [%message (option : Bigint.t option)])
        [@nontail]);
      [%expect
        {|
        (option (-16))
        (option (240))
        (option (-2004846480))
        (option (5496718387884602416))
        (option (61373803910015629365604881137405268496))
        |}]
    ;;

    let%expect_test "find" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring = Base_bigstring.of_string string_0_31 in
        int_kinds ~f:(fun kind ~of_int:_ ~to_int ->
          let option =
            Bigstring.find
              parallel
              (Bigstring.with_kind_exn kind bigstring)
              ~f:(fun _ a ->
                let a = to_int a in
                Bigint.(a % of_int 4 = zero))
            |> Option.map ~f:(fun i -> to_int i)
          in
          print_s [%message (option : Bigint.Hex.t option)])
        [@nontail]);
      [%expect
        {|
        (option (0x0))
        (option (0x100))
        (option (0x3020100))
        (option (0x706050403020100))
        (option (0xf0e0d0c0b0a09080706050403020100))
        |}]
    ;;

    let%expect_test "splitting bigstring with parallel for over slices" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        let bigstring =
          Arrays.Bigstring.init parallel (Int8, 10) ~f:(fun _ i -> Int8.of_int (i + 1))
        in
        let pivots = [: 1; 2; 3; 4; 5; 6; 7; 8; 9 :] in
        let slice = Arrays.Bigstring.Slice.slice bigstring in
        Arrays.Bigstring.Slice.for_ ~pivots parallel slice ~f:(fun _ slice ->
          let bigstring = Arrays.Bigstring.of_slice slice in
          let x = Arrays.Bigstring.get bigstring 0 in
          Arrays.Bigstring.set bigstring 0 (Int8.add x 1s));
        dump bigstring);
      [%expect
        {|
        (bigstring
         ("00000000  02 03 04 05 06 07 08 09  0a 0b                    |..........|"))
        |}]
    ;;

    let%expect_test "sort_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          Bigstring.sort_inplace parallel bigstring ~compare:(fun _ a b ->
            Bigint.compare (to_int a) (to_int b));
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
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  21 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |!...............|"
          "00000030  22 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |\"...............|"
          "00000040  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000050  28 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |(...............|"
          "00000060  29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |)...............|"
          "00000070  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000080  40 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |@...............|"
          "00000090  62 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |b...............|"))
        |}]
    ;;

    let%expect_test "sort" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          let bigstring =
            Bigstring.sort parallel bigstring ~compare:(fun _ a b ->
              Bigint.compare (to_int a) (to_int b))
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
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  21 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |!...............|"
          "00000030  22 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |\"...............|"
          "00000040  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000050  28 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |(...............|"
          "00000060  29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |)...............|"
          "00000070  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000080  40 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |@...............|"
          "00000090  62 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |b...............|"))
        |}]
    ;;

    let%expect_test "stable_sort_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          Bigstring.stable_sort_inplace parallel bigstring ~compare:(fun _ a b ->
            Bigint.compare (to_int a) (to_int b));
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
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  21 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |!...............|"
          "00000030  22 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |\"...............|"
          "00000040  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000050  28 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |(...............|"
          "00000060  29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |)...............|"
          "00000070  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000080  40 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |@...............|"
          "00000090  62 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |b...............|"))
        |}]
    ;;

    let%expect_test "stable_sort" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          let bigstring =
            Bigstring.stable_sort parallel bigstring ~compare:(fun _ a b ->
              Bigint.compare (to_int a) (to_int b))
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
        (bigstring
         ("00000000  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  21 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |!...............|"
          "00000030  22 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |\"...............|"
          "00000040  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000050  28 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |(...............|"
          "00000060  29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |)...............|"
          "00000070  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000080  40 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |@...............|"
          "00000090  62 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |b...............|"))
        |}]
    ;;

    let%expect_test "scan_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 25) |> Bigint.of_int |> of_int)
          in
          let total =
            Bigstring.scan_inplace
              parallel
              bigstring
              ~init:(of_int Bigint.zero)
              ~f:(fun _ a b -> Bigint.( + ) (to_int a) (to_int b) |> of_int)
            |> to_int
          in
          dump bigstring;
          print_s [%sexp ~~(total : Bigint.t)])
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  00 0e 17 26 36 3e 49 60  67 79                    |...&6>I`gy|"))
        (total 123)
        (bigstring
         ("00000000  00 00 0e 00 17 00 26 00  36 00 3e 00 49 00 60 00  |......&.6.>.I.`.|"
          "00000010  67 00 79 00                                       |g.y.|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 0e 00 00 00  17 00 00 00 26 00 00 00  |............&...|"
          "00000010  36 00 00 00 3e 00 00 00  49 00 00 00 60 00 00 00  |6...>...I...`...|"
          "00000020  67 00 00 00 79 00 00 00                           |g...y...|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  0e 00 00 00 00 00 00 00  |................|"
          "00000010  17 00 00 00 00 00 00 00  26 00 00 00 00 00 00 00  |........&.......|"
          "00000020  36 00 00 00 00 00 00 00  3e 00 00 00 00 00 00 00  |6.......>.......|"
          "00000030  49 00 00 00 00 00 00 00  60 00 00 00 00 00 00 00  |I.......`.......|"
          "00000040  67 00 00 00 00 00 00 00  79 00 00 00 00 00 00 00  |g.......y.......|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  0e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  17 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000030  26 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |&...............|"
          "00000040  36 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |6...............|"
          "00000050  3e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |>...............|"
          "00000060  49 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |I...............|"
          "00000070  60 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |`...............|"
          "00000080  67 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |g...............|"
          "00000090  79 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |y...............|"))
        (total 123)
        |}]
    ;;

    let%expect_test "scan" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 25) |> Bigint.of_int |> of_int)
          in
          let bigstring, total =
            Bigstring.scan parallel bigstring ~init:(of_int Bigint.zero) ~f:(fun _ a b ->
              Bigint.( + ) (to_int a) (to_int b) |> of_int)
            |> Tuple2.map_snd ~f:to_int
          in
          dump bigstring;
          print_s [%sexp ~~(total : Bigint.t)])
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  00 0e 17 26 36 3e 49 60  67 79                    |...&6>I`gy|"))
        (total 123)
        (bigstring
         ("00000000  00 00 0e 00 17 00 26 00  36 00 3e 00 49 00 60 00  |......&.6.>.I.`.|"
          "00000010  67 00 79 00                                       |g.y.|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 0e 00 00 00  17 00 00 00 26 00 00 00  |............&...|"
          "00000010  36 00 00 00 3e 00 00 00  49 00 00 00 60 00 00 00  |6...>...I...`...|"
          "00000020  67 00 00 00 79 00 00 00                           |g...y...|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  0e 00 00 00 00 00 00 00  |................|"
          "00000010  17 00 00 00 00 00 00 00  26 00 00 00 00 00 00 00  |........&.......|"
          "00000020  36 00 00 00 00 00 00 00  3e 00 00 00 00 00 00 00  |6.......>.......|"
          "00000030  49 00 00 00 00 00 00 00  60 00 00 00 00 00 00 00  |I.......`.......|"
          "00000040  67 00 00 00 00 00 00 00  79 00 00 00 00 00 00 00  |g.......y.......|"))
        (total 123)
        (bigstring
         ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  0e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  17 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000030  26 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |&...............|"
          "00000040  36 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |6...............|"
          "00000050  3e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |>...............|"
          "00000060  49 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |I...............|"
          "00000070  60 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |`...............|"
          "00000080  67 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |g...............|"
          "00000090  79 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |y...............|"))
        (total 123)
        |}]
    ;;

    let%expect_test "scan_inclusive_inplace" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 25) |> Bigint.of_int |> of_int)
          in
          Bigstring.scan_inclusive_inplace
            parallel
            bigstring
            ~init:(of_int Bigint.zero)
            ~f:(fun _ a b -> Bigint.( + ) (to_int a) (to_int b) |> of_int);
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  0e 17 26 36 3e 49 60 67  79 7b                    |..&6>I`gy{|"))
        (bigstring
         ("00000000  0e 00 17 00 26 00 36 00  3e 00 49 00 60 00 67 00  |....&.6.>.I.`.g.|"
          "00000010  79 00 7b 00                                       |y.{.|"))
        (bigstring
         ("00000000  0e 00 00 00 17 00 00 00  26 00 00 00 36 00 00 00  |........&...6...|"
          "00000010  3e 00 00 00 49 00 00 00  60 00 00 00 67 00 00 00  |>...I...`...g...|"
          "00000020  79 00 00 00 7b 00 00 00                           |y...{...|"))
        (bigstring
         ("00000000  0e 00 00 00 00 00 00 00  17 00 00 00 00 00 00 00  |................|"
          "00000010  26 00 00 00 00 00 00 00  36 00 00 00 00 00 00 00  |&.......6.......|"
          "00000020  3e 00 00 00 00 00 00 00  49 00 00 00 00 00 00 00  |>.......I.......|"
          "00000030  60 00 00 00 00 00 00 00  67 00 00 00 00 00 00 00  |`.......g.......|"
          "00000040  79 00 00 00 00 00 00 00  7b 00 00 00 00 00 00 00  |y.......{.......|"))
        (bigstring
         ("00000000  0e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  17 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  26 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |&...............|"
          "00000030  36 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |6...............|"
          "00000040  3e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |>...............|"
          "00000050  49 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |I...............|"
          "00000060  60 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |`...............|"
          "00000070  67 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |g...............|"
          "00000080  79 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |y...............|"
          "00000090  7b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |{...............|"))
        |}]
    ;;

    let%expect_test "scan_inclusive" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 25) |> Bigint.of_int |> of_int)
          in
          let bigstring =
            Bigstring.scan_inclusive
              parallel
              bigstring
              ~init:(of_int Bigint.zero)
              ~f:(fun _ a b -> Bigint.( + ) (to_int a) (to_int b) |> of_int)
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  0e 17 26 36 3e 49 60 67  79 7b                    |..&6>I`gy{|"))
        (bigstring
         ("00000000  0e 00 17 00 26 00 36 00  3e 00 49 00 60 00 67 00  |....&.6.>.I.`.g.|"
          "00000010  79 00 7b 00                                       |y.{.|"))
        (bigstring
         ("00000000  0e 00 00 00 17 00 00 00  26 00 00 00 36 00 00 00  |........&...6...|"
          "00000010  3e 00 00 00 49 00 00 00  60 00 00 00 67 00 00 00  |>...I...`...g...|"
          "00000020  79 00 00 00 7b 00 00 00                           |y...{...|"))
        (bigstring
         ("00000000  0e 00 00 00 00 00 00 00  17 00 00 00 00 00 00 00  |................|"
          "00000010  26 00 00 00 00 00 00 00  36 00 00 00 00 00 00 00  |&.......6.......|"
          "00000020  3e 00 00 00 00 00 00 00  49 00 00 00 00 00 00 00  |>.......I.......|"
          "00000030  60 00 00 00 00 00 00 00  67 00 00 00 00 00 00 00  |`.......g.......|"
          "00000040  79 00 00 00 00 00 00 00  7b 00 00 00 00 00 00 00  |y.......{.......|"))
        (bigstring
         ("00000000  0e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000010  17 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  26 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |&...............|"
          "00000030  36 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |6...............|"
          "00000040  3e 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |>...............|"
          "00000050  49 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |I...............|"
          "00000060  60 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |`...............|"
          "00000070  67 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |g...............|"
          "00000080  79 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |y...............|"
          "00000090  7b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |{...............|"))
        |}]
    ;;

    let%expect_test "filter" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          let bigstring =
            Bigstring.filter parallel bigstring ~f:(fun _ a ->
              Bigint.( < ) (to_int a) (Bigint.of_int 50))
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  22 28 29 21 24 07 2b 1b                           |\"()!$.+.|"))
        (bigstring
         ("00000000  22 00 28 00 29 00 21 00  24 00 07 00 2b 00 1b 00  |\".(.).!.$...+...|"))
        (bigstring
         ("00000000  22 00 00 00 28 00 00 00  29 00 00 00 21 00 00 00  |\"...(...)...!...|"
          "00000010  24 00 00 00 07 00 00 00  2b 00 00 00 1b 00 00 00  |$.......+.......|"))
        (bigstring
         ("00000000  22 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  |\".......(.......|"
          "00000010  29 00 00 00 00 00 00 00  21 00 00 00 00 00 00 00  |).......!.......|"
          "00000020  24 00 00 00 00 00 00 00  07 00 00 00 00 00 00 00  |$...............|"
          "00000030  2b 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |+...............|"))
        (bigstring
         ("00000000  22 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |\"...............|"
          "00000010  28 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |(...............|"
          "00000020  29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |)...............|"
          "00000030  21 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |!...............|"
          "00000040  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000050  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000060  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000070  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"))
        |}]
    ;;

    let%expect_test "filteri" =
      Scheduler.parallel scheduler ~f:(fun parallel ->
        int_kinds ~f:(fun kind ~of_int ~to_int ->
          let bigstring =
            Bigstring.init parallel (kind, 10) ~f:(fun _ i ->
              Base.Int.(hash i % 100) |> Bigint.of_int |> of_int)
          in
          let bigstring =
            Bigstring.filteri parallel bigstring ~f:(fun _ i a ->
              i >= 5 && Bigint.( < ) (to_int a) (Bigint.of_int 50))
          in
          dump bigstring)
        [@nontail]);
      [%expect
        {|
        (bigstring
         ("00000000  24 07 2b 1b                                       |$.+.|"))
        (bigstring
         ("00000000  24 00 07 00 2b 00 1b 00                           |$...+...|"))
        (bigstring
         ("00000000  24 00 00 00 07 00 00 00  2b 00 00 00 1b 00 00 00  |$.......+.......|"))
        (bigstring
         ("00000000  24 00 00 00 00 00 00 00  07 00 00 00 00 00 00 00  |$...............|"
          "00000010  2b 00 00 00 00 00 00 00  1b 00 00 00 00 00 00 00  |+...............|"))
        (bigstring
         ("00000000  24 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |$...............|"
          "00000010  07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
          "00000020  2b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |+...............|"
          "00000030  1b 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"))
        |}]
    ;;
  end
end

include Common.Test_schedulers (Test_scheduler)
