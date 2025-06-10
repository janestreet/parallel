open! Base
open! Import
include Parallel_arrays_intf

type%template 'a modality = { modality : 'a @@ m }
[@@unboxed] [@@modality m = (uncontended, shared)]

type ('a, 'b) f = Parallel_kernel.t @ local -> int -> 'a @ portable -> 'b @ portable

let magic_uncontended = (Obj.magic_uncontended [@mode many portable aliased])

let[@inline] wrap : f:('a @ portable -> 'b @ portable) @ portable -> ('a, 'b) f @ portable
  =
  fun ~f _ _ a -> f a
;;

let[@inline] wrapi
  : f:(int -> 'a @ portable -> 'b @ portable) @ portable -> ('a, 'b) f @ portable
  =
  fun ~f _ i a -> f i a
;;

let[@inline] wrap'
  :  f:(Parallel_kernel.t @ local -> 'a @ portable -> 'b @ portable) @ portable
  -> ('a, 'b) f @ portable
  =
  fun ~f parallel _ a -> f parallel a
;;

let[@inline] split ~grain ~i ~j =
  let n = j - i in
  if n <= grain
  then Pair_or_null.none ()
  else (
    let pivot = i + (n / 2) in
    Pair_or_null.some (~i, ~j:pivot) (~i:pivot, ~j))
;;

(** These modules make use of [unsafe_racy_set_contended] / [unsafe_racy_get_contended],
    which ignore contention. The usage is safe because it is never possible for two fibers
    to have acccess to the same array index at the same time, and we know that the
    elements of a parallel array do not share any unsynchronized state.

    Most functions take an uncontended array and may return an uncontended array. This is
    also safe because if the parallel operation shares array elements with other capsules,
    they become contended. When the parallel operation returns, we know we still have the
    only uncontended reference to the array and its contents. *)

module Make_init (Array : sig
  @@ portable
    type 'a t
    type 'a mut
    type 'a init

    val empty_like : 'a init -> 'a t @ portable
    val to_length : 'a init -> int
    val create_for_init : 'a init -> 'a @ portable -> 'a mut @ portable
    val freeze : 'a mut @ portable -> 'a t @ portable
    val unsafe_racy_set_contended : 'a mut @ contended -> int -> 'a @ portable -> unit
  end) =
struct
  let init_gen ~grain parallel init ~f =
    if grain < 1 then invalid_arg "grain < 1";
    match Array.to_length init with
    | length when length < 0 -> invalid_arg "length < 0"
    | 0 -> Array.empty_like init
    | length ->
      let output = Array.create_for_init init (f parallel 0) in
      let f parallel i =
        let a = f parallel i in
        Array.unsafe_racy_set_contended output i a
      in
      Parallel_kernel.for_ ~grain parallel ~start:1 ~stop:length ~f;
      Array.freeze output
  ;;

  let[@inline] init' ?(grain = 16) parallel init ~f = init_gen ~grain parallel init ~f

  let[@inline] init ?(grain = 16) parallel init ~f =
    init' ~grain parallel init ~f:(fun _ i -> f i) [@nontail]
  ;;
end

module Make_inplace (Array : sig
  @@ portable
    type 'a t
    type 'a mut

    val length : 'a t @ contended -> int
    val freeze : 'a mut @ portable -> 'a t @ portable
    val copy : 'a t @ portable -> 'a mut @ portable
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
    val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a @ portable -> unit
  end) =
struct
  let mapi_inplace_gen ~grain parallel input ~f ~i ~j =
    if grain < 1 then invalid_arg "grain < 1";
    let f parallel i =
      let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
      let b = f parallel i a in
      Array.unsafe_racy_set_contended input i b
    in
    Parallel_kernel.for_ ~grain parallel ~start:i ~stop:j ~f [@nontail]
  ;;

  let[@inline] mapi_inplace' ?(grain = 16) parallel input ~f =
    mapi_inplace_gen ~grain parallel input ~f ~i:0 ~j:(Array.length input) [@nontail]
  ;;

  let[@inline] mapi_inplace ?(grain = 16) parallel input ~f =
    mapi_inplace' ~grain parallel input ~f:(wrapi ~f) [@nontail]
  ;;

  let[@inline] map_inplace' ?(grain = 16) parallel input ~f =
    mapi_inplace' ~grain parallel input ~f:(wrap' ~f) [@nontail]
  ;;

  let[@inline] map_inplace ?(grain = 16) parallel input ~f =
    mapi_inplace' ~grain parallel input ~f:(wrap ~f) [@nontail]
  ;;

  let init_inplace_gen ~grain parallel input ~f ~i ~j =
    if grain < 1 then invalid_arg "grain < 1";
    let f parallel i =
      let a = f parallel i in
      Array.unsafe_racy_set_contended input i a
    in
    Parallel_kernel.for_ ~grain parallel ~start:i ~stop:j ~f [@nontail]
  ;;

  let[@inline] init_inplace' ?(grain = 16) parallel input ~f =
    init_inplace_gen ~grain parallel input ~f ~i:0 ~j:(Array.length input)
  ;;

  let[@inline] init_inplace ?(grain = 16) parallel input ~f =
    init_inplace' ~grain parallel input ~f:(fun _ i -> f i) [@nontail]
  ;;

  module Sort = struct
    (* Adapted from [Base.Array.sort]. Uses inclusive bounds. *)

    let[@inline] swap input i j =
      let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
      let b = Array.unsafe_racy_get_contended input j |> magic_uncontended in
      Array.unsafe_racy_set_contended input i b;
      Array.unsafe_racy_set_contended input j a
    ;;

    let[@inline] [@loop always] rec heapify parallel input ~compare root ~left ~right =
      let relative_root = root - left in
      let left_child = (2 * relative_root) + left + 1 in
      let right_child = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right
           &&
           let left_child =
             Array.unsafe_racy_get_contended input left_child |> magic_uncontended
           in
           let root = Array.unsafe_racy_get_contended input root |> magic_uncontended in
           compare parallel left_child root > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right
           &&
           let right_child =
             Array.unsafe_racy_get_contended input right_child |> magic_uncontended
           in
           let largest =
             Array.unsafe_racy_get_contended input largest |> magic_uncontended
           in
           compare parallel right_child largest > 0
        then right_child
        else largest
      in
      if largest <> root
      then (
        swap input root largest;
        heapify parallel input ~compare largest ~left ~right)
    ;;

    let[@inline] build_heap parallel input ~compare ~left ~right =
      for i = (left + right) / 2 downto left do
        heapify parallel input ~compare i ~left ~right
      done
    ;;

    let heap_sort parallel input ~compare ~left ~right =
      build_heap parallel input ~compare ~left ~right;
      for i = right downto left + 1 do
        swap input left i;
        heapify parallel input ~compare left ~left ~right:(i - 1)
      done
    ;;

    let[@inline] [@loop always] rec insert parallel input ~left ~compare i v =
      let i_next = i - 1 in
      if i_next >= left
         &&
         let a = Array.unsafe_racy_get_contended input i_next |> magic_uncontended in
         compare parallel a v > 0
      then (
        let a = Array.unsafe_racy_get_contended input i_next |> magic_uncontended in
        Array.unsafe_racy_set_contended input i a;
        insert parallel input ~left ~compare i_next v)
      else i
    ;;

    let insertion_sort parallel input ~compare ~left ~right =
      for pos = left + 1 to right do
        let v = Array.unsafe_racy_get_contended input pos |> magic_uncontended in
        let final_pos = insert parallel input ~left ~compare pos v in
        Array.unsafe_racy_set_contended input final_pos v
      done
    ;;

    let[@inline] five_element_sort parallel input ~compare m1 m2 m3 m4 m5 =
      let compare_and_swap i j =
        let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
        let b = Array.unsafe_racy_get_contended input j |> magic_uncontended in
        if compare parallel a b > 0 then swap input i j
      in
      compare_and_swap m1 m2;
      compare_and_swap m4 m5;
      compare_and_swap m1 m3;
      compare_and_swap m2 m3;
      compare_and_swap m1 m4;
      compare_and_swap m3 m4;
      compare_and_swap m2 m5;
      compare_and_swap m2 m3;
      compare_and_swap m4 m5 [@nontail]
    ;;

    let[@inline] choose_pivots parallel input ~compare ~left ~right =
      let sixth = (right - left) / 6 in
      let m1 = left + sixth in
      let m2 = m1 + sixth in
      let m3 = m2 + sixth in
      let m4 = m3 + sixth in
      let m5 = m4 + sixth in
      five_element_sort parallel input ~compare m1 m2 m3 m4 m5;
      let m2_val = Array.unsafe_racy_get_contended input m2 |> magic_uncontended in
      let m3_val = Array.unsafe_racy_get_contended input m3 |> magic_uncontended in
      let m4_val = Array.unsafe_racy_get_contended input m4 |> magic_uncontended in
      if compare parallel m2_val m3_val = 0
      then #(m2_val, m3_val, true)
      else if compare parallel m3_val m4_val = 0
      then #(m3_val, m4_val, true)
      else #(m2_val, m4_val, false)
    ;;

    let[@inline] dual_pivot_partition parallel input ~compare ~left ~right =
      let #(pivot1, pivot2, pivots_equal) =
        choose_pivots parallel input ~compare ~left ~right
      in
      let[@loop always] rec loop l p r =
        let pv = Array.unsafe_racy_get_contended input p |> magic_uncontended in
        if compare parallel pv pivot1 < 0
        then (
          swap input p l;
          cont (l + 1) (p + 1) r)
        else if compare parallel pv pivot2 > 0
        then (
          let[@loop always] rec scan_backwards r =
            if r > p
               &&
               let a = Array.unsafe_racy_get_contended input r |> magic_uncontended in
               compare parallel a pivot2 > 0
            then scan_backwards (r - 1)
            else r
          in
          let r = scan_backwards r in
          swap input r p;
          cont l p (r - 1))
        else cont l (p + 1) r
      and cont l p r = if p > r then #(l, r) else loop l p r in
      let #(l, r) = cont left left right in
      #(l, r, pivots_equal)
    ;;

    let rec sort_inplace_gen ~depth ~grain parallel input #(left, right) ~compare =
      let n = right - left + 1 in
      if n <= 32
      then insertion_sort parallel input ~left ~right ~compare
      else if depth = 0
      then heap_sort parallel input ~left ~right ~compare
      else (
        let depth = depth - 1 in
        let #(mid0, mid1, middle_sorted) =
          dual_pivot_partition parallel input ~left ~right ~compare
        in
        match
          ( middle_sorted
          , mid0 - left >= grain || right - mid1 >= grain || mid1 - mid0 >= grain )
        with
        | true, true ->
          let (), () =
            Parallel_kernel.fork_join2
              parallel
              (fun parallel ->
                sort_inplace_gen ~depth ~grain parallel input #(left, mid0 - 1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth ~grain parallel input #(mid1 + 1, right) ~compare)
          in
          ()
        | false, true ->
          let (), (), () =
            Parallel_kernel.fork_join3
              parallel
              (fun parallel ->
                sort_inplace_gen ~depth ~grain parallel input #(left, mid0 - 1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth ~grain parallel input #(mid0, mid1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth ~grain parallel input #(mid1 + 1, right) ~compare)
          in
          ()
        | true, false ->
          sort_inplace_gen ~depth ~grain parallel input #(left, mid0 - 1) ~compare;
          sort_inplace_gen ~depth ~grain parallel input #(mid1 + 1, right) ~compare
        | false, false ->
          sort_inplace_gen ~depth ~grain parallel input #(left, mid0 - 1) ~compare;
          sort_inplace_gen ~depth ~grain parallel input #(mid0, mid1) ~compare;
          sort_inplace_gen ~depth ~grain parallel input #(mid1 + 1, right) ~compare)
    ;;
  end

  let[@inline] sort_inplace' ?(grain = 16) parallel input ~compare =
    if grain < 1 then invalid_arg "grain < 1";
    Sort.sort_inplace_gen
      ~depth:32
      ~grain
      parallel
      input
      #(0, Array.length input - 1)
      ~compare
  ;;

  let[@inline] sort_inplace ?(grain = 16) parallel input ~compare =
    sort_inplace' ~grain parallel input ~compare:(fun _ a b -> compare a b) [@nontail]
  ;;

  module Stable_sort = struct
    let[@inline] blit input output ~at ~i ~j =
      for idx = 0 to j - i - 1 do
        let a = Array.unsafe_racy_get_contended input (i + idx) |> magic_uncontended in
        Array.unsafe_racy_set_contended output (at + idx) a
      done
    ;;

    let[@inline] [@loop always] rec sequential_merge
      parallel
      ~input
      ~output
      ~at
      ~i0
      ~j0
      ~i1
      ~j1
      ~compare
      =
      if i0 = j0
      then blit input output ~at ~i:i1 ~j:j1
      else if i1 = j1
      then blit input output ~at ~i:i0 ~j:j0
      else (
        let a = Array.unsafe_racy_get_contended input i0 |> magic_uncontended in
        let b = Array.unsafe_racy_get_contended input i1 |> magic_uncontended in
        if compare parallel a b <= 0
        then (
          Array.unsafe_racy_set_contended output at a;
          sequential_merge
            parallel
            ~input
            ~output
            ~at:(at + 1)
            ~i0:(i0 + 1)
            ~j0
            ~i1
            ~j1
            ~compare)
        else (
          Array.unsafe_racy_set_contended output at b;
          sequential_merge
            parallel
            ~input
            ~output
            ~at:(at + 1)
            ~i0
            ~j0
            ~i1:(i1 + 1)
            ~j1
            ~compare))
    ;;

    let[@inline] [@loop always] rec binary_search parallel input ~i ~j a ~compare =
      let n = j - i in
      if n = 1
      then (
        let b = Array.unsafe_racy_get_contended input i |> magic_uncontended in
        if compare parallel a b <= 0 then i else j)
      else (
        let pivot = i + (n / 2) in
        let b = Array.unsafe_racy_get_contended input pivot |> magic_uncontended in
        if compare parallel a b <= 0
        then binary_search parallel input ~i ~j:pivot a ~compare
        else binary_search parallel input ~i:pivot ~j a ~compare)
    ;;

    let[@inline] rec parallel_merge
      ~grain
      parallel
      ~input
      ~output
      ~at
      ~i0
      ~j0
      ~i1
      ~j1
      ~compare
      =
      let n0 = j0 - i0 in
      let n1 = j1 - i1 in
      if n0 > 1 && n1 > 1 && (n0 >= grain || n1 >= grain)
      then (
        let pivot0 = i0 + (n0 / 2) in
        let pivot1 = Array.unsafe_racy_get_contended input pivot0 |> magic_uncontended in
        let pivot1 = binary_search parallel input ~i:i1 ~j:j1 pivot1 ~compare in
        let len = pivot0 - i0 + (pivot1 - i1) in
        let (), () =
          Parallel_kernel.fork_join2
            parallel
            (fun parallel ->
              parallel_merge
                ~grain
                parallel
                ~input
                ~output
                ~at
                ~i0
                ~j0:pivot0
                ~i1
                ~j1:pivot1
                ~compare)
            (fun parallel ->
              parallel_merge
                ~grain
                parallel
                ~input
                ~output
                ~at:(at + len)
                ~i0:pivot0
                ~j0
                ~i1:pivot1
                ~j1
                ~compare)
        in
        ())
      else sequential_merge parallel ~input ~output ~at ~i0 ~j0 ~i1 ~j1 ~compare
    ;;

    let rec sort_inplace_gen ~grain parallel ~input ~output ~i ~j ~compare =
      let n = j - i in
      if n <= 32
      then Sort.insertion_sort parallel output ~left:i ~right:(j - 1) ~compare
      else (
        let pivot = i + (n / 2) in
        if n / 2 >= grain
        then (
          let (), () =
            Parallel_kernel.fork_join2
              parallel
              (fun parallel ->
                sort_inplace_gen
                  ~grain
                  parallel
                  ~input:output
                  ~output:input
                  ~i
                  ~j:pivot
                  ~compare)
              (fun parallel ->
                sort_inplace_gen
                  ~grain
                  parallel
                  ~input:output
                  ~output:input
                  ~i:pivot
                  ~j
                  ~compare)
          in
          parallel_merge
            ~grain
            parallel
            ~input
            ~output
            ~at:i
            ~i0:i
            ~j0:pivot
            ~i1:pivot
            ~j1:j
            ~compare)
        else (
          sort_inplace_gen
            ~grain
            parallel
            ~input:output
            ~output:input
            ~i
            ~j:pivot
            ~compare;
          sort_inplace_gen
            ~grain
            parallel
            ~input:output
            ~output:input
            ~i:pivot
            ~j
            ~compare;
          sequential_merge
            parallel
            ~input
            ~output
            ~at:i
            ~i0:i
            ~j0:pivot
            ~i1:pivot
            ~j1:j
            ~compare))
    ;;
  end

  let[@inline] stable_sort_inplace' ?(grain = 16) parallel input ~compare =
    if grain < 1 then invalid_arg "grain < 1";
    Stable_sort.sort_inplace_gen
      ~grain
      parallel
      ~input:(Array.copy input |> Array.freeze)
      ~output:input
      ~i:0
      ~j:(Array.length input)
      ~compare
  ;;

  let[@inline] stable_sort_inplace ?(grain = 16) parallel input ~compare =
    stable_sort_inplace' ~grain parallel input ~compare:(fun _ a b -> compare a b)
    [@nontail]
  ;;
end

module Make_map (Array : sig
  @@ portable
    type 'a t
    type 'a mut

    val empty : unit -> 'a t @ portable
    val length : 'a t @ contended -> int
    val freeze : 'a mut @ portable -> 'a t @ portable
    val create_for_map : _ t @ portable shared -> 'a @ portable -> 'a mut @ portable
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
    val unsafe_racy_set_contended : 'a mut @ contended -> int -> 'a @ portable -> unit
  end) =
struct
  let mapi_gen ~grain parallel input ~f =
    if grain < 1 then invalid_arg "grain < 1";
    let length = Array.length input in
    if length = 0
    then Array.empty ()
    else (
      let first = Array.unsafe_racy_get_contended input 0 |> magic_uncontended in
      let output = Array.create_for_map input (f parallel 0 first) in
      let f parallel i =
        let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
        let b = f parallel i a in
        Array.unsafe_racy_set_contended output i b
      in
      Parallel_kernel.for_ ~grain parallel ~start:1 ~stop:length ~f;
      Array.freeze output)
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] mapi' ?(grain = 16) parallel input ~f = mapi_gen ~grain parallel input ~f

  let[@inline] mapi ?(grain = 16) parallel input ~f =
    (mapi' [@mode m]) ~grain parallel input ~f:(wrapi ~f) [@nontail]
  ;;

  let[@inline] map' ?(grain = 16) parallel input ~f =
    (mapi' [@mode m]) ~grain parallel input ~f:(wrap' ~f) [@nontail]
  ;;

  let[@inline] map ?(grain = 16) parallel input ~f =
    (mapi' [@mode m]) ~grain parallel input ~f:(wrap ~f) [@nontail]
  ;;]

  let mapi2_exn_gen ~grain parallel input0 input1 ~f =
    if grain < 1 then invalid_arg "grain < 1";
    let length0 = Array.length input0 in
    let length1 = Array.length input1 in
    if length0 <> length1 then invalid_arg "mismatched lengths";
    if length0 = 0
    then Array.empty ()
    else (
      let a = Array.unsafe_racy_get_contended input0 0 |> magic_uncontended in
      let b = Array.unsafe_racy_get_contended input1 0 |> magic_uncontended in
      let output = Array.create_for_map input0 (f parallel 0 a b) in
      let f parallel i =
        let a = Array.unsafe_racy_get_contended input0 i |> magic_uncontended in
        let b = Array.unsafe_racy_get_contended input1 i |> magic_uncontended in
        let c = f parallel i a b in
        Array.unsafe_racy_set_contended output i c
      in
      Parallel_kernel.for_ ~grain parallel ~start:0 ~stop:length0 ~f;
      Array.freeze output)
  ;;

  [%%template
  [@@@mode.default a = (uncontended, shared), b = (uncontended, shared)]

  let mapi2_exn' ?(grain = 16) parallel input0 input1 ~f =
    mapi2_exn_gen ~grain parallel input0 input1 ~f
  ;;

  let[@inline] mapi2_exn ?(grain = 16) parallel input0 input1 ~f =
    mapi2_exn' ~grain parallel input0 input1 ~f:(fun [@inline] _ i a b -> f i a b)
    [@nontail]
  ;;

  let[@inline] map2_exn' ?(grain = 16) parallel input0 input1 ~f =
    mapi2_exn' ~grain parallel input0 input1 ~f:(fun [@inline] parallel _ a b ->
      f parallel a b)
    [@nontail]
  ;;

  let[@inline] map2_exn ?(grain = 16) parallel input0 input1 ~f =
    mapi2_exn' ~grain parallel input0 input1 ~f:(fun [@inline] _ _ a b -> f a b)
    [@nontail]
  ;;]
end

module Make_reduce (Array : sig
  @@ portable
    type 'a t

    val length : 'a t @ contended -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
  end) =
struct
  let iteri_gen ~grain parallel input ~f =
    if grain < 1 then invalid_arg "grain < 1";
    let f parallel i =
      let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
      f parallel i a
    in
    Parallel_kernel.for_ ~grain parallel ~start:0 ~stop:(Array.length input) ~f [@nontail]
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] iteri' ?(grain = 16) parallel input ~f = iteri_gen parallel ~grain input ~f

  let[@inline] iteri ?(grain = 16) parallel input ~f =
    iteri' ~grain parallel ~f:(wrapi ~f) input [@nontail]
  ;;

  let[@inline] iter' ?(grain = 16) parallel input ~f =
    iteri' ~grain parallel ~f:(wrap' ~f) input [@nontail]
  ;;

  let[@inline] iter ?(grain = 16) parallel input ~f =
    iteri' ~grain parallel ~f:(wrap ~f) input [@nontail]
  ;;]

  let foldi_gen ~grain parallel input ~init ~f ~combine ~i ~j =
    Parallel_kernel.fold
      ~grain
      parallel
      ~init:#(init, (~i, ~j))
      ~next:(fun parallel acc (~i, ~j) ->
        if i = j
        then Pair_or_null.none ()
        else (
          let a = Array.unsafe_racy_get_contended input i |> magic_uncontended in
          Pair_or_null.some (f parallel i acc a) (~i:(i + 1), ~j)))
      ~fork:(fun _ (~i, ~j) -> split ~grain ~i ~j)
      ~join:combine [@nontail]
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] foldi' ?(grain = 16) parallel input ~init ~f ~combine =
    if grain < 1 then invalid_arg "grain < 1";
    foldi_gen ~grain parallel input ~init ~f ~combine ~i:0 ~j:(Array.length input)
  ;;

  let[@inline] foldi ?(grain = 16) parallel input ~init ~f ~combine =
    (foldi' [@mode m])
      ~grain
      parallel
      input
      ~init
      ~f:(fun _ i acc a -> f i acc a)
      ~combine:(fun _ a b -> combine a b) [@nontail]
  ;;

  let[@inline] fold' ?(grain = 16) parallel input ~init ~f ~combine =
    (foldi' [@mode m])
      ~grain
      parallel
      input
      ~init
      ~f:(fun parallel _ acc a -> f parallel acc a)
      ~combine [@nontail]
  ;;

  let[@inline] fold ?(grain = 16) parallel input ~init ~f ~combine =
    (foldi' [@mode m])
      ~grain
      parallel
      input
      ~init
      ~f:(fun _ _ a -> f a)
      ~combine:(fun _ a b -> combine a b) [@nontail]
  ;;]

  let[@inline] reduce' ?(grain = 16) parallel input ~f =
    foldi'
      ~grain
      parallel
      input
      ~init:None
      ~f:(fun parallel _ acc a ->
        match acc with
        | Some acc -> Some (f parallel acc a)
        | None -> Some a)
      ~combine:(fun parallel a b ->
        Option.merge_portable_contended ~f:(fun a b -> f parallel a b) a b [@nontail])
  ;;

  let[@inline] reduce ?(grain = 16) parallel input ~f =
    reduce' ~grain parallel input ~f:(fun _ a b -> f a b) [@nontail]
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] findi' ?(grain = 16) parallel t ~f =
    (foldi' [@mode m])
      ~grain
      parallel
      t
      ~init:None
      ~f:(fun parallel i acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel i a then Some a else None)
      ~combine:(fun _ a b -> Option.first_some_portable_contended a b) [@nontail]
  ;;

  let[@inline] findi ?(grain = 16) parallel t ~f =
    (findi' [@mode m]) ~grain parallel t ~f:(wrapi ~f) [@nontail]
  ;;

  let[@inline] find' ?(grain = 16) parallel t ~f =
    (findi' [@mode m]) ~grain parallel t ~f:(wrap' ~f) [@nontail]
  ;;

  let[@inline] find ?(grain = 16) parallel t ~f =
    (findi' [@mode m]) ~grain parallel t ~f:(wrap ~f) [@nontail]
  ;;]
end

module Make_sort (Array : sig
  @@ portable
    type 'a t
    type 'a mut

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val wrap : 'a mut @ m portable -> ('a modality[@mode m]) mut @ m portable
    val unwrap : ('a modality[@mode m]) mut @ m portable -> 'a mut @ m portable
    val copy : 'a t @ m portable -> 'a mut @ m portable
    val freeze : 'a mut @ m portable -> 'a t @ m portable]

    val sort_inplace'
      :  ?grain:int
      -> Parallel_kernel.t @ local
      -> 'a mut @ portable
      -> compare:(Parallel_kernel.t @ local -> 'a @ portable -> 'a @ portable -> int)
         @ portable
      -> unit

    val stable_sort_inplace'
      :  ?grain:int
      -> Parallel_kernel.t @ local
      -> 'a mut @ portable
      -> compare:(Parallel_kernel.t @ local -> 'a @ portable -> 'a @ portable -> int)
         @ portable
      -> unit
  end) =
struct
  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] sort' ?(grain = 16) parallel input ~compare =
    (* Safety: [output] is a fresh copy of the array, which is inherently uncontended.
       However, it contains elements which might be [shared], so we first [wrap] them in a
       [shared] modality (if necessary), and then [magic_uncontended] the whole array. *)
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> magic_uncontended
    in
    Array.sort_inplace'
      ~grain
      parallel
      output
      ~compare:(fun parallel { modality = a } { modality = b } -> compare parallel a b);
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m])
  ;;

  let[@inline] sort ?(grain = 16) parallel input ~compare =
    (sort' [@mode m]) ~grain parallel input ~compare:(fun _ a b -> compare a b) [@nontail]
  ;;

  let[@inline] stable_sort' ?(grain = 16) parallel input ~compare =
    (* Safety: [output] is a fresh copy of the array, which is inherently uncontended.
       However, it contains elements which might be [shared], so we first [wrap] them in a
       [shared] modality (if necessary), and then [magic_uncontended] the whole array. *)
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> magic_uncontended
    in
    Array.stable_sort_inplace'
      ~grain
      parallel
      output
      ~compare:(fun parallel { modality = a } { modality = b } -> compare parallel a b);
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m])
  ;;

  let[@inline] stable_sort ?(grain = 16) parallel input ~compare =
    (stable_sort' [@mode m]) ~grain parallel input ~compare:(fun _ a b -> compare a b)
    [@nontail]
  ;;]
end

module%template Make_slice (Array : sig
  @@ portable
    type 'a t : k with 'a portable

    val length : 'a t @ contended -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
  end) =
struct
  type 'a t =
    { array : 'a Array.t @@ contended global portable
    ; start : int
    ; stop : int
    }

  let length { start; stop; _ } = stop - start

  [@@@mode.default m = (uncontended, shared)]

  let slice ?i ?j array = exclave_
    let len = Array.length array in
    let i = Option.value i ~default:0 in
    let j = Option.value j ~default:len in
    if i < 0 || i > j || j > len then invalid_arg "invalid index range";
    { array; start = i; stop = j }
  ;;

  let sub ?i ?j t = exclave_
    let len = length t in
    let i = Option.value i ~default:0 in
    let j = Option.value j ~default:len in
    if i < 0 || i > j || j > len then invalid_arg "invalid index range";
    { array = t.array; start = t.start + i; stop = t.start + j }
  ;;

  let get { array; start; stop } i =
    if i < 0 || i >= stop - start then invalid_arg "index out of bounds";
    let a = Array.unsafe_racy_get_contended array (start + i) |> magic_uncontended in
    a
  ;;

  let unsafe_get { array; start; _ } i =
    let a = Array.unsafe_racy_get_contended array (start + i) |> magic_uncontended in
    a
  ;;

  let fork_join2 parallel ?pivot { array; start; stop } f1 f2 =
    let len = stop - start in
    if len = 0 then invalid_arg "empty slice";
    let pivot = Option.value pivot ~default:(len / 2) in
    if pivot < 0 || pivot > len then invalid_arg "index out of bounds";
    Parallel_kernel.fork_join2
      parallel
      (fun parallel -> f1 parallel { array; start; stop = start + pivot })
      (fun parallel -> f2 parallel { array; start = start + pivot; stop }) [@nontail]
  ;;
end
[@@kind k = (mutable_data, immutable_data)]

module Make_islice (Array : sig
  @@ portable
    type 'a t : immutable_data with 'a portable

    val length : 'a t @ contended -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
  end) =
struct
  module%template Slice = Make_slice [@kind immutable_data] (Array)
end

module Make_slice (Array : sig
  @@ portable
    type 'a t : mutable_data with 'a portable

    val length : 'a t @ contended -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended portable
    val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a @ portable -> unit
  end) =
struct
  module%template Islice = Make_slice [@kind mutable_data] (Array)

  module Slice = struct
    include Islice

    let set { array; start; stop } i a =
      if i < 0 || i >= stop - start then invalid_arg "index out of bounds";
      Array.unsafe_racy_set_contended array (start + i) a
    ;;

    let unsafe_set { array; start; _ } i a =
      Array.unsafe_racy_set_contended array (start + i) a
    ;;

    let set' t i f = set t i (f ())
    let unsafe_set' t i f = unsafe_set t i (f ())
  end
end

module Array = struct
  include Array

  type nonrec 'a t = 'a portable t
  type 'a init = int
  type 'a mut = 'a t

  let[@inline] unsafe_racy_get_contended t i = (unsafe_racy_get_contended t i).portable

  let[@inline] unsafe_racy_set_contended t i a =
    unsafe_racy_set_contended t i { portable = a }
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external wrap
    :  'a t @ m portable
    -> ('a modality[@mode m]) t @ m portable
    @@ portable
    = "%identity"

  external unwrap
    :  ('a modality[@mode m]) t @ m portable
    -> 'a t @ m portable
    @@ portable
    = "%identity"

  external of_array
    : ('a : value mod contended portable).
    'a array @ m -> 'a portable array @ m
    @@ portable
    = "%identity"

  external to_array
    : ('a : value mod contended portable).
    'a portable array @ m -> 'a array @ m
    @@ portable
    = "%identity"

  let[@inline] freeze mut = mut
  let[@inline] get t i = ((get [@mode m]) t i).portable
  let[@inline] unsafe_get t i = ((unsafe_get [@mode m]) t i).portable]

  let[@inline] set t i a = set t i { portable = a }
  let[@inline] unsafe_set t i a = unsafe_set t i { portable = a }
  let[@inline] set' t i f = set t i (f ())
  let[@inline] unsafe_set' t i f = unsafe_set t i (f ())
  let[@inline] to_length init = init
  let[@inline] create_for_init n a = create ~len:n { portable = a }
  let[@inline] create_for_map t a = create ~len:(length t) { portable = a }
  let[@inline] empty_like _ = empty ()

  include functor Make_slice
  include functor Make_init
  include functor Make_inplace
  include functor Make_map
  include functor Make_reduce
  include functor Make_sort
end

module Iarray = struct
  include Iarray

  type nonrec 'a t = 'a portable t
  type 'a init = int
  type 'a mut = 'a portable array

  let[@inline] unsafe_racy_get_contended t i = (unsafe_racy_get_contended t i).portable
  let unsafe_racy_set_contended = Array.unsafe_racy_set_contended

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external of_iarray
    : ('a : value mod contended portable).
    'a iarray @ m -> 'a portable iarray @ m
    @@ portable
    = "%identity"

  external to_iarray
    : ('a : value mod contended portable).
    'a portable iarray @ m -> 'a iarray @ m
    @@ portable
    = "%identity"

  let wrap = (Array.wrap [@mode m])
  let unwrap = (Array.unwrap [@mode m])

  let[@inline] copy t =
    (unsafe_to_array__promise_no_mutation [@mode m]) t |> (Array.copy [@mode m])
  ;;

  let freeze = (unsafe_of_array__promise_no_mutation [@mode m])
  let[@inline] get t i = ((get [@mode m]) t i).portable
  let[@inline] unsafe_get t i = ((unsafe_get [@mode m]) t i).portable]

  let[@inline] to_length init = init
  let[@inline] create_for_init n a = Array.create ~len:n { portable = a }
  let[@inline] create_for_map t a = Array.create ~len:(length t) { portable = a }
  let[@inline] empty_like _ = empty ()
  let sort_inplace' = Array.sort_inplace'
  let stable_sort_inplace' = Array.stable_sort_inplace'

  include functor Make_islice
  include functor Make_init
  include functor Make_map
  include functor Make_reduce
  include functor Make_sort
end

module Vec = struct
  include Vec

  type nonrec 'a t = 'a portable t
  type 'a init = int
  type 'a mut = 'a t

  let[@inline] unsafe_racy_get_contended t i = (unsafe_racy_get_contended t i).portable

  let[@inline] unsafe_racy_set_contended t i a =
    unsafe_racy_set_contended t i { portable = a }
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external wrap
    :  'a t @ m portable
    -> ('a modality[@mode m]) t @ m portable
    @@ portable
    = "%identity"

  external unwrap
    :  ('a modality[@mode m]) t @ m portable
    -> 'a t @ m portable
    @@ portable
    = "%identity"

  external of_vec
    : ('a : value mod contended portable).
    'a Vec.t @ m -> 'a portable Vec.t @ m
    @@ portable
    = "%identity"

  external to_vec
    : ('a : value mod contended portable).
    'a portable Vec.t @ m -> 'a Vec.t @ m
    @@ portable
    = "%identity"

  let[@inline] freeze mut = mut
  let[@inline] get t i = ((get [@mode m]) t i).portable
  let[@inline] unsafe_get t i = ((unsafe_get [@mode m]) t i).portable]

  let[@inline] set t i a = set t i { portable = a }
  let[@inline] unsafe_set t i a = unsafe_set t i { portable = a }
  let[@inline] set' t i f = set t i (f ())
  let[@inline] unsafe_set' t i f = unsafe_set t i (f ())
  let[@inline] to_length init = init
  let[@inline] create_for_init n a = create ~len:n { portable = a }
  let[@inline] create_for_map t a = create ~len:(length t) { portable = a }
  let[@inline] empty_like _ = empty ()

  include functor Make_slice
  include functor Make_init
  include functor Make_inplace
  include functor Make_map
  include functor Make_reduce
  include functor Make_sort
end

module Bigstring = struct
  include Bigstring

  type 'a mut = 'a t
  type 'a init = 'a Kind.t * int

  let unsafe_racy_get_contended = Expert.unsafe_racy_get_contended
  let unsafe_racy_set_contended = Expert.unsafe_racy_set_contended

  let create_for_init (kind, n) a =
    let bigstring = create kind n in
    unsafe_racy_set_contended bigstring 0 a;
    bigstring
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external wrap
    :  'a t @ m portable
    -> ('a modality[@mode m]) t @ m portable
    @@ portable
    = "%identity"

  external unwrap
    :  ('a modality[@mode m]) t @ m portable
    -> 'a t @ m portable
    @@ portable
    = "%identity"

  let freeze t = t
  let get = get
  let unsafe_get = unsafe_get]

  let set' t i f = set t i (f ())
  let unsafe_set' t i f = unsafe_set t i (f ())
  let to_length (_, n) = n
  let empty_like (kind, _) = empty kind

  include functor Make_slice
  include functor Make_inplace
  include functor Make_reduce
  include functor Make_init
  include functor Make_sort
end
