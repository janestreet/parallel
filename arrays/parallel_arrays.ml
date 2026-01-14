open! Base
open! Import
include Parallel_arrays_intf

type%template 'a modality = { modality : 'a @@ m }
[@@unboxed] [@@modality m = (uncontended, shared)]

type ('a, 'b) f = Parallel_kernel.t @ local -> int -> 'a -> 'b

let[@inline] wrap
  : ('a : value mod portable) ('b : value mod portable).
  f:(Parallel_kernel.t @ local -> 'a -> 'b) @ shareable -> ('a, 'b) f @ shareable
  =
  fun ~f parallel _ a -> f parallel a
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
    type ('a : value mod portable) t
    type ('a : value mod portable) mut : mutable_data with 'a
    type 'a init

    val empty_like : 'a init -> 'a t
    val to_length : 'a init -> int
    val create : 'a init -> 'a -> 'a mut
    val freeze : 'a mut -> 'a t
    val unsafe_racy_set_contended : 'a mut @ contended -> int -> 'a -> unit
  end) =
struct
  let init_gen parallel init ~f =
    match Array.to_length init with
    | length when length < 0 -> invalid_arg "length < 0"
    | 0 -> Array.empty_like init
    | length ->
      let first = f parallel 0 in
      let output = Array.create init first in
      let f parallel i =
        let a = f parallel i in
        Array.unsafe_racy_set_contended output i a
      in
      Parallel_kernel.for_ parallel ~start:1 ~stop:length ~f;
      Array.freeze output
  ;;

  let[@inline] init parallel init ~f = init_gen parallel init ~f
end

module Make_inplace (Array : sig
  @@ portable
    type ('a : value mod portable) t : mutable_data with 'a

    val copy : 'a t -> 'a t
    val length : 'a t @ shared -> int
    val create_like : 'a t @ shared -> len:int -> 'a -> 'a t
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
    val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a -> unit
  end) =
struct
  let mapi_inplace_gen parallel input ~f ~i ~j =
    let f parallel i =
      let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
      let b = f parallel i a in
      Array.unsafe_racy_set_contended input i b
    in
    Parallel_kernel.for_ parallel ~start:i ~stop:j ~f [@nontail]
  ;;

  let[@inline] mapi_inplace parallel input ~f =
    mapi_inplace_gen parallel input ~f ~i:0 ~j:(Array.length input) [@nontail]
  ;;

  let[@inline] map_inplace parallel input ~f =
    mapi_inplace parallel input ~f:(wrap ~f) [@nontail]
  ;;

  let init_inplace_gen parallel input ~f ~i ~j =
    let f parallel i =
      let a = f parallel i in
      Array.unsafe_racy_set_contended input i a
    in
    Parallel_kernel.for_ parallel ~start:i ~stop:j ~f [@nontail]
  ;;

  let[@inline] init_inplace parallel input ~f =
    init_inplace_gen parallel input ~f ~i:0 ~j:(Array.length input)
  ;;

  module Sort = struct
    (* Adapted from [Base.Array.sort]. Uses inclusive bounds. *)

    let[@inline] swap input i j =
      let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
      let b = Array.unsafe_racy_get_contended input j |> Obj.magic_uncontended in
      Array.unsafe_racy_set_contended input i b;
      Array.unsafe_racy_set_contended input j a
    ;;

    let[@inline] [@loop] rec heapify parallel input ~compare root ~left ~right =
      let relative_root = root - left in
      let left_child = (2 * relative_root) + left + 1 in
      let right_child = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right
           &&
           let left_child =
             Array.unsafe_racy_get_contended input left_child |> Obj.magic_uncontended
           in
           let root =
             Array.unsafe_racy_get_contended input root |> Obj.magic_uncontended
           in
           compare parallel left_child root > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right
           &&
           let right_child =
             Array.unsafe_racy_get_contended input right_child |> Obj.magic_uncontended
           in
           let largest =
             Array.unsafe_racy_get_contended input largest |> Obj.magic_uncontended
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

    let[@inline] [@loop] rec insert parallel input ~left ~compare i v =
      let i_next = i - 1 in
      if i_next >= left
         &&
         let a = Array.unsafe_racy_get_contended input i_next |> Obj.magic_uncontended in
         compare parallel a v > 0
      then (
        let a = Array.unsafe_racy_get_contended input i_next |> Obj.magic_uncontended in
        Array.unsafe_racy_set_contended input i a;
        insert parallel input ~left ~compare i_next v)
      else i
    ;;

    let insertion_sort parallel input ~compare ~left ~right =
      for pos = left + 1 to right do
        let v = Array.unsafe_racy_get_contended input pos |> Obj.magic_uncontended in
        let final_pos = insert parallel input ~left ~compare pos v in
        Array.unsafe_racy_set_contended input final_pos v
      done
    ;;

    let[@inline] five_element_sort parallel input ~compare m1 m2 m3 m4 m5 =
      let compare_and_swap i j =
        let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
        let b = Array.unsafe_racy_get_contended input j |> Obj.magic_uncontended in
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
      let m2_val = Array.unsafe_racy_get_contended input m2 |> Obj.magic_uncontended in
      let m3_val = Array.unsafe_racy_get_contended input m3 |> Obj.magic_uncontended in
      let m4_val = Array.unsafe_racy_get_contended input m4 |> Obj.magic_uncontended in
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
      let[@loop] rec loop l p r =
        let pv = Array.unsafe_racy_get_contended input p |> Obj.magic_uncontended in
        if compare parallel pv pivot1 < 0
        then (
          swap input p l;
          cont (l + 1) (p + 1) r)
        else if compare parallel pv pivot2 > 0
        then (
          let[@loop] rec scan_backwards r =
            if r > p
               &&
               let a = Array.unsafe_racy_get_contended input r |> Obj.magic_uncontended in
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

    let rec sort_inplace_gen ~depth parallel input #(left, right) ~compare =
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
        match middle_sorted with
        | true ->
          let #((), ()) =
            Parallel_kernel.fork_join2
              parallel
              (fun parallel ->
                sort_inplace_gen ~depth parallel input #(left, mid0 - 1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth parallel input #(mid1 + 1, right) ~compare)
          in
          ()
        | false ->
          let #((), (), ()) =
            Parallel_kernel.fork_join3
              parallel
              (fun parallel ->
                sort_inplace_gen ~depth parallel input #(left, mid0 - 1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth parallel input #(mid0, mid1) ~compare)
              (fun parallel ->
                sort_inplace_gen ~depth parallel input #(mid1 + 1, right) ~compare)
          in
          ())
    ;;
  end

  let[@inline] sort_inplace parallel input ~compare =
    Sort.sort_inplace_gen ~depth:32 parallel input #(0, Array.length input - 1) ~compare
  ;;

  module Stable_sort = struct
    let[@inline] blit input output ~at ~i ~j =
      for idx = 0 to j - i - 1 do
        let a =
          Array.unsafe_racy_get_contended input (i + idx) |> Obj.magic_uncontended
        in
        Array.unsafe_racy_set_contended output (at + idx) a
      done
    ;;

    let[@inline] [@loop] rec sequential_merge
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
        let a = Array.unsafe_racy_get_contended input i0 |> Obj.magic_uncontended in
        let b = Array.unsafe_racy_get_contended input i1 |> Obj.magic_uncontended in
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

    let[@inline] [@loop] rec binary_search parallel input ~i ~j a ~compare =
      let n = j - i in
      if n = 1
      then (
        let b = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
        if compare parallel a b <= 0 then i else j)
      else (
        let pivot = i + (n / 2) in
        let b = Array.unsafe_racy_get_contended input pivot |> Obj.magic_uncontended in
        if compare parallel a b <= 0
        then binary_search parallel input ~i ~j:pivot a ~compare
        else binary_search parallel input ~i:pivot ~j a ~compare)
    ;;

    let[@inline] rec parallel_merge parallel ~input ~output ~at ~i0 ~j0 ~i1 ~j1 ~compare =
      let n0 = j0 - i0 in
      let n1 = j1 - i1 in
      if n0 >= 16 && n1 >= 16
      then (
        let pivot0 = i0 + (n0 / 2) in
        let pivot1 =
          Array.unsafe_racy_get_contended input pivot0 |> Obj.magic_uncontended
        in
        let pivot1 = binary_search parallel input ~i:i1 ~j:j1 pivot1 ~compare in
        let len = pivot0 - i0 + (pivot1 - i1) in
        let #((), ()) =
          Parallel_kernel.fork_join2
            parallel
            (fun parallel ->
              parallel_merge
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

    let rec sort_inplace_gen parallel ~input ~output ~i ~j ~compare =
      let n = j - i in
      if n <= 32
      then Sort.insertion_sort parallel output ~left:i ~right:(j - 1) ~compare
      else (
        let pivot = i + (n / 2) in
        let #((), ()) =
          Parallel_kernel.fork_join2
            parallel
            (fun parallel ->
              sort_inplace_gen parallel ~input:output ~output:input ~i ~j:pivot ~compare)
            (fun parallel ->
              sort_inplace_gen parallel ~input:output ~output:input ~i:pivot ~j ~compare)
        in
        parallel_merge
          parallel
          ~input
          ~output
          ~at:i
          ~i0:i
          ~j0:pivot
          ~i1:pivot
          ~j1:j
          ~compare)
    ;;
  end

  let[@inline] stable_sort_inplace parallel input ~compare =
    Stable_sort.sort_inplace_gen
      parallel
      ~input:(Array.copy input)
      ~output:input
      ~i:0
      ~j:(Array.length input)
      ~compare
  ;;

  (** For every pair [l, r] of consecutive elements in the [target] range
      [\[target_pos, target_pos + 2 * len)], compute [f l r] and write it to the the
      [scratch] range [\[scratch_pos, scratch_pos + len)]. *)
  let[@inline] contract parallel ~target ~target_pos ~len ~scratch ~scratch_pos ~f =
    let[@inline] contract parallel i =
      let l =
        Array.unsafe_racy_get_contended target (target_pos + (2 * i))
        |> Obj.magic_uncontended
      in
      let r =
        Array.unsafe_racy_get_contended target (target_pos + (2 * i) + 1)
        |> Obj.magic_uncontended
      in
      Array.unsafe_racy_set_contended scratch (scratch_pos + i) (f parallel l r)
    in
    Parallel_kernel.for_ parallel ~start:0 ~stop:len ~f:contract [@nontail]
  ;;

  (** At this point, the element at index [i] in the [scratch] range
      [\[scratch_pos, scratch_pos + len)] represents the prefix sum of the [target] range
      [\[target_pos, target_pos + target_offset + 2 * i)]. We copy it to the index
      [target_pos + target_offset + 2 * i] in the [target] range, then fold it with the
      following element and write the result to the following index. *)
  let[@inline] expand
    parallel
    ~target
    ~target_pos
    ~target_offset
    ~len
    ~scratch
    ~scratch_pos
    ~f
    =
    let[@inline] expand parallel i =
      let contracted_elem =
        Array.unsafe_racy_get_contended scratch (scratch_pos + i) |> Obj.magic_uncontended
      in
      let to_fold =
        Array.unsafe_racy_get_contended target (target_pos + target_offset + (2 * i))
        |> Obj.magic_uncontended
      in
      Array.unsafe_racy_set_contended target (target_pos + (2 * i)) contracted_elem;
      Array.unsafe_racy_set_contended
        target
        (target_pos + (2 * i) + 1)
        (f parallel contracted_elem to_fold)
    in
    Parallel_kernel.for_ parallel ~start:0 ~stop:len ~f:expand [@nontail]
  ;;

  (** This computes the exclusive prefix sums of the [target] range
      [\[target_pos, target_pos + len)] in-place, using the [scratch] range
      [\[scratch_pos, scratch_pos + len / 2)] as scratch space. [target] and [scratch] may
      refer to the same array, as long as the aforementioned ranges do not overlap.

      First, we [contract] the [target] range into the [scratch] range, effectively
      halving the number of elements to consider. Then, we recursively compute the
      exclusive prefix sums of the [scratch] range. Finally, we copy the elements of the
      [scratch] range back to the even indices of the [target] range, and recover the odd
      indices of the [target] range by applying [f] to the prefix sum at the preceding
      even index and the original element at the odd index. *)
  let rec scan_inplace_gen
    parallel
    ~target
    ~target_pos
    ~len
    ~scratch
    ~scratch_pos
    ~init
    ~f
    =
    match len with
    | 0 -> init
    | 1 ->
      let first =
        Array.unsafe_racy_get_contended target target_pos |> Obj.magic_uncontended
      in
      Array.unsafe_racy_set_contended target target_pos init;
      first
    | len ->
      let contracted_len = len / 2 in
      contract parallel ~target ~target_pos ~len:contracted_len ~scratch ~scratch_pos ~f;
      let partial_result =
        scan_inplace_gen
          parallel
          ~target:scratch
          ~target_pos:scratch_pos
          ~len:contracted_len
          ~scratch
          ~scratch_pos:(scratch_pos + contracted_len)
          ~init
          ~f
      in
      expand
        parallel
        ~target
        ~target_pos
        ~target_offset:0
        ~len:contracted_len
        ~scratch
        ~scratch_pos
        ~f;
      (* Account for last element of an odd length input, which does not get contracted. *)
      if len land 1 = 1
      then (
        let last =
          Array.unsafe_racy_get_contended target (target_pos + len - 1)
          |> Obj.magic_uncontended
        in
        Array.unsafe_racy_set_contended target (target_pos + len - 1) partial_result;
        f parallel partial_result last)
      else partial_result
  ;;

  let[@inline] scan_inplace parallel input ~init ~f =
    let len = Array.length input in
    let scratch = Array.create_like input ~len init in
    scan_inplace_gen
      parallel
      ~target:input
      ~target_pos:0
      ~len
      ~scratch
      ~scratch_pos:0
      ~init
      ~f
  ;;

  (** This algorithm is similar to [scan_inplace_gen'] but with indices offset by 1. *)
  let rec scan_inclusive_inplace_gen
    parallel
    ~target
    ~target_pos
    ~len
    ~scratch
    ~scratch_pos
    ~init
    ~f
    =
    if len <= 1
    then ()
    else (
      let contracted_len = len / 2 in
      contract parallel ~target ~target_pos ~len:contracted_len ~scratch ~scratch_pos ~f;
      scan_inclusive_inplace_gen
        parallel
        ~target:scratch
        ~target_pos:scratch_pos
        ~len:contracted_len
        ~scratch
        ~scratch_pos:(scratch_pos + contracted_len)
        ~init
        ~f;
      expand
        parallel
        ~target
        ~target_pos:(target_pos + 1)
        ~target_offset:1
        ~len:(contracted_len - 1)
        ~scratch
        ~scratch_pos
        ~f;
      (* Need to special-case the first and last elements, since contracted index [i] is
         used to derive expanded indices [2*i + 1] and [2*i + 2]. Note the first element
         remains unchanged. *)
      let contracted_last =
        Array.unsafe_racy_get_contended scratch (scratch_pos + contracted_len - 1)
        |> Obj.magic_uncontended
      in
      Array.unsafe_racy_set_contended
        target
        (target_pos + (2 * contracted_len) - 1)
        contracted_last;
      if len land 1 = 1
      then (
        let target_last =
          Array.unsafe_racy_get_contended target (target_pos + len - 1)
          |> Obj.magic_uncontended
        in
        Array.unsafe_racy_set_contended
          target
          (target_pos + len - 1)
          (f parallel contracted_last target_last)))
  ;;

  let[@inline] scan_inclusive_inplace parallel input ~init ~f =
    let len = Array.length input in
    let scratch = Array.create_like input ~len init in
    scan_inclusive_inplace_gen
      parallel
      ~target:input
      ~target_pos:0
      ~len
      ~scratch
      ~scratch_pos:0
      ~init
      ~f
  ;;
end

module%template Make_map (Array : sig
  @@ portable
    type ('a : k mod portable) t
    type ('a : k mod portable) mut : mutable_data with 'a

    val empty : ('a : k mod portable). unit -> 'a t
    val length : ('a : k mod portable). 'a t @ shared -> int
    val create_like : ('a : k mod portable). _ t @ shared -> len:int -> 'a -> 'a mut
    val freeze : ('a : k mod portable). 'a mut -> 'a t

    val unsafe_racy_get_contended
      : ('a : k mod portable).
      'a t @ contended -> int -> 'a @ contended

    val unsafe_racy_set_contended
      : ('a : k mod portable).
      'a mut @ contended -> int -> 'a -> unit
  end) =
struct
  let mapi_gen parallel input ~f =
    let length = Array.length input in
    if length = 0
    then Array.empty ()
    else (
      let first =
        let first = Array.unsafe_racy_get_contended input 0 |> Obj.magic_uncontended in
        f parallel 0 first
      in
      let output = Array.create_like input ~len:length first in
      let f parallel i =
        let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
        let b = f parallel i a in
        Array.unsafe_racy_set_contended output i b
      in
      Parallel_kernel.for_ parallel ~start:1 ~stop:length ~f;
      Array.freeze output)
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] mapi parallel input ~f = mapi_gen parallel input ~f

  let[@inline] map parallel input ~f =
    (mapi [@mode m]) parallel input ~f:(wrap ~f) [@nontail]
  ;;]

  let mapi2_exn_gen parallel input0 input1 ~f =
    let length0 = Array.length input0 in
    let length1 = Array.length input1 in
    if length0 <> length1 then invalid_arg "mismatched lengths";
    if length0 = 0
    then Array.empty ()
    else (
      let first =
        let a = Array.unsafe_racy_get_contended input0 0 |> Obj.magic_uncontended in
        let b = Array.unsafe_racy_get_contended input1 0 |> Obj.magic_uncontended in
        f parallel 0 a b
      in
      let output = Array.create_like input0 ~len:length0 first in
      let f parallel i =
        let a = Array.unsafe_racy_get_contended input0 i |> Obj.magic_uncontended in
        let b = Array.unsafe_racy_get_contended input1 i |> Obj.magic_uncontended in
        let c = f parallel i a b in
        Array.unsafe_racy_set_contended output i c
      in
      Parallel_kernel.for_ parallel ~start:1 ~stop:length0 ~f;
      Array.freeze output)
  ;;

  [%%template
  [@@@mode.default a = (uncontended, shared), b = (uncontended, shared)]

  let mapi2_exn parallel input0 input1 ~f = mapi2_exn_gen parallel input0 input1 ~f

  let[@inline] map2_exn parallel input0 input1 ~f =
    mapi2_exn parallel input0 input1 ~f:(fun [@inline] parallel _ a b -> f parallel a b)
    [@nontail]
  ;;]
end
[@@kind k = (value, value_or_null mod separable)]

module Make_reduce (Array : sig
  @@ portable
    type ('a : value mod portable) t

    val length : 'a t @ shared -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  end) =
struct
  let iteri_gen parallel input ~f =
    let f parallel i =
      let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
      f parallel i a
    in
    Parallel_kernel.for_ parallel ~start:0 ~stop:(Array.length input) ~f [@nontail]
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] iteri parallel input ~f = iteri_gen parallel input ~f
  let[@inline] iter parallel input ~f = iteri parallel ~f:(wrap ~f) input [@nontail]]

  let foldi_gen parallel input ~init ~f ~combine ~i ~j =
    (Parallel_kernel.fold [@kind value_or_null (value_or_null & value_or_null)])
      parallel
      ~init
      ~state:(#(~i, ~j) : #(i:int * j:int))
      ~next:(fun parallel acc #(~i, ~j) ->
        if i = j
        then
          (Option_u.none
          [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
            ()
        else (
          let a = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
          (Option_u.some
          [@kind (_ : (_ : value_or_null & (value_or_null & value_or_null)))])
            #(f parallel i acc a, #(~i:(i + 1), ~j))))
      ~stop:(fun _ acc -> acc)
      ~fork:(fun _ #(~i, ~j) ->
        let n = j - i in
        if n <= 1
        then
          (Option_u.none
          [@kind
            (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null)))])
            ()
        else (
          let pivot = i + (n / 2) in
          (Option_u.some
          [@kind
            (_ : (_ : (value_or_null & value_or_null) & (value_or_null & value_or_null)))])
            #(#(~i, ~j:pivot), #(~i:pivot, ~j))))
      ~join:combine [@nontail]
  ;;

  let[@inline] foldi parallel input ~init ~f ~combine =
    foldi_gen parallel input ~init ~f ~combine ~i:0 ~j:(Array.length input)
  ;;

  let[@inline] fold parallel input ~init ~f ~combine =
    foldi
      parallel
      input
      ~init
      ~f:(fun parallel _ acc a -> f parallel acc a)
      ~combine [@nontail]
  ;;

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] reduce parallel input ~f =
    foldi
      parallel
      input
      ~init:(fun () : ('a modality[@mode m]) option -> None)
      ~f:(fun parallel _ acc a ->
        match acc with
        | Some { modality = acc } -> Some { modality = f parallel acc a }
        | None -> Some { modality = a })
      ~combine:(fun parallel a b ->
        Option.merge
          ~f:(fun { modality = a } { modality = b } -> { modality = f parallel a b })
          a
          b [@nontail])
    |> function
    | Some ({ modality = a } : ('a modality[@mode m])) -> Some a
    | None -> None
  ;;

  let[@inline] min_elt parallel t ~compare =
    (reduce [@mode m]) parallel t ~f:(fun parallel a b ->
      let is_leq = compare parallel a b <= 0 in
      (Bool.select [@mode m]) is_leq a b)
    [@nontail]
  ;;

  let[@inline] max_elt parallel t ~compare =
    (reduce [@mode m]) parallel t ~f:(fun parallel a b ->
      let is_geq = compare parallel a b >= 0 in
      (Bool.select [@mode m]) is_geq a b)
    [@nontail]
  ;;

  let[@inline] findi parallel (t : 'a Array.t) ~f =
    foldi
      parallel
      t
      ~init:(fun () : 'a option -> None)
      ~f:(fun parallel i acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel i a then Some a else None)
      ~combine:(fun _ a b -> Option.first_some a b) [@nontail]
  ;;

  let[@inline] find parallel t ~f = (findi [@mode m]) parallel t ~f:(wrap ~f) [@nontail]]
end

module Make_sort (Array : sig
  @@ portable
    type ('a : value mod portable) t
    type ('a : value mod portable) mut : mutable_data with 'a

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val wrap : 'a mut @ m -> ('a modality[@mode m]) mut @ m
    val unwrap : ('a modality[@mode m]) mut @ m -> 'a mut @ m
    val copy : 'a t @ m -> 'a mut @ m
    val freeze : 'a mut @ m -> 'a t @ m]

    val sort_inplace
      :  Parallel_kernel.t @ local
      -> 'a mut
      -> compare:
           (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
         @ shareable
      -> unit

    val stable_sort_inplace
      :  Parallel_kernel.t @ local
      -> 'a mut
      -> compare:
           (Parallel_kernel.t @ local -> 'a @ local shared -> 'a @ local shared -> int)
         @ shareable
      -> unit
  end) =
struct
  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] sort parallel input ~compare =
    (* Safety: [output] is a fresh copy of the array, which is inherently uncontended.
       However, it contains elements which might be [shared], so we first [wrap] them in a
       [shared] modality (if necessary), and then [magic_uncontended] the whole array. *)
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> Obj.magic_uncontended
    in
    Array.sort_inplace
      parallel
      output
      ~compare:(fun parallel { modality = a } { modality = b } -> compare parallel a b);
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m])
  ;;

  let[@inline] stable_sort parallel input ~compare =
    (* Safety: [output] is a fresh copy of the array, which is inherently uncontended.
       However, it contains elements which might be [shared], so we first [wrap] them in a
       [shared] modality (if necessary), and then [magic_uncontended] the whole array. *)
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> Obj.magic_uncontended
    in
    Array.stable_sort_inplace
      parallel
      output
      ~compare:(fun parallel { modality = a } { modality = b } -> compare parallel a b);
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m])
  ;;]
end

module Make_scan (Array : sig
  @@ portable
    type ('a : value mod portable) t
    type ('a : value mod portable) mut : mutable_data with 'a

    val scan_inplace
      :  Parallel_kernel.t @ local
      -> 'a mut
      -> init:'a
      -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a) @ shareable
      -> 'a

    val scan_inclusive_inplace
      :  Parallel_kernel.t @ local
      -> 'a mut
      -> init:'a
      -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'a @ shared -> 'a) @ shareable
      -> unit

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val wrap : 'a mut @ m -> ('a modality[@mode m]) mut @ m
    val unwrap : ('a modality[@mode m]) mut @ m -> 'a mut @ m
    val copy : 'a t @ m -> 'a mut @ m
    val freeze : 'a mut @ m -> 'a t @ m]
  end) =
struct
  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let scan parallel input ~init ~f =
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> Obj.magic_uncontended
    in
    let result =
      Array.scan_inplace
        parallel
        output
        ~init:{ modality = init }
        ~f:(fun parallel { modality = a } { modality = b } ->
          { modality = f parallel a b })
    in
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m]), result.modality
  ;;

  let scan_inclusive parallel input ~init ~f =
    let output =
      (Array.copy [@mode m]) input |> (Array.wrap [@mode m]) |> Obj.magic_uncontended
    in
    Array.scan_inclusive_inplace
      parallel
      output
      ~init:{ modality = init }
      ~f:(fun parallel { modality = a } { modality = b } -> { modality = f parallel a b });
    (Array.unwrap [@mode m]) output |> (Array.freeze [@mode m])
  ;;]
end

module Bigstring0 = struct
  include Bigstring

  type 'a mut = 'a t
  type 'a init = 'a Kind.t * int

  let unsafe_racy_get_contended = Expert.unsafe_racy_get_contended
  let unsafe_racy_set_contended = Expert.unsafe_racy_set_contended

  let create (kind, n) a =
    let bigstring = Bigstring.create kind n in
    unsafe_racy_set_contended bigstring 0 a;
    bigstring
  ;;

  let[@inline] create_like t ~len a = create (kind t, len) a

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

  let[@inline] freeze t = t]

  let[@inline] to_length (_, n) = n
  let[@inline] empty_like (kind, _) = empty kind

  include functor Make_inplace
  include functor Make_init
  include functor Make_scan
end

module%template Ints = struct
  module type S = sig @@ portable
    type t : immutable_data

    val zero : t
    val one : t
    val max_int : int
    val not_eq : t -> t -> bool
    val to_int_exn : t -> int
    val add : t -> t -> t
  end

  module I8 : S with type t = int8 = struct
    type t = int8

    let zero = 0s
    let one = 1s
    let max_int = Int8.max_int |> Int8.to_int
    let not_eq x y = not (Int8.equal x y)
    let to_int_exn = Int8.to_int
    let add = Int8.add
  end

  module I16 : S with type t = int16 = struct
    type t = int16

    let zero = 0S
    let one = 1S
    let max_int = Int16.max_int |> Int16.to_int
    let not_eq x y = not (Int16.equal x y)
    let to_int_exn = Int16.to_int
    let add = Int16.add
  end

  module I32 : S with type t = int32 = struct
    type t = int32

    let zero = 0l
    let one = 1l
    let max_int = Int32.max_value |> Int32.to_int_exn
    let not_eq = Int32.( <> )
    let to_int_exn = Int32.to_int_exn
    let add = [%eta2 Int32.( + )]
  end

  module I64 : S with type t = int64 = struct
    type t = int64

    let zero = 0L
    let one = 1L
    let max_int = Int.max_value
    let not_eq = Int64.( <> )
    let to_int_exn = Int64.to_int_exn
    let add = [%eta2 Int64.( + )]
  end
end

module%template Make_filter (Array : sig
  @@ portable
    type ('a : k mod portable) t
    type ('a : k mod portable) mut : mutable_data with 'a

    val length : ('a : k mod portable). 'a t @ shared -> int
    val create_like : ('a : k mod portable). 'a t @ shared -> len:int -> 'a -> 'a mut
    val freeze : ('a : k mod portable). 'a mut -> 'a t

    val unsafe_racy_get_contended
      : ('a : k mod portable).
      'a t @ contended -> int -> 'a @ contended

    val unsafe_racy_set_contended
      : ('a : k mod portable).
      'a mut @ contended -> int -> 'a -> unit
  end) =
struct
  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let[@inline] filteri_gen
    (type int : immutable_data)
    (module Int : Ints.S with type t = int)
    ~kind
    parallel
    input
    ~f
    =
    let length = Array.length input in
    (* Create 1/0 array for elements to be kept *)
    let keep =
      Bigstring0.init parallel (kind, length) ~f:(fun parallel i ->
        let input_i = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
        Bool.select (f parallel i input_i) Int.one Int.zero)
    in
    (* Determine target index for each element *)
    let filter_len =
      Bigstring0.scan_inplace parallel keep ~init:Int.zero ~f:(fun _ x y -> Int.add x y)
    in
    let output =
      Array.create_like
        input
        ~len:(Int.to_int_exn filter_len)
        (Array.unsafe_racy_get_contended input 0 |> Obj.magic_uncontended)
    in
    (* For each element, move it into output if it is kept *)
    Parallel_kernel.for_ parallel ~start:0 ~stop:(length - 1) ~f:(fun _ i ->
      let keep_i = Bigstring0.unsafe_racy_get_contended keep i in
      let keep_next = Bigstring0.unsafe_racy_get_contended keep (i + 1) in
      if Int.not_eq keep_i keep_next
      then (
        let input_i = Array.unsafe_racy_get_contended input i |> Obj.magic_uncontended in
        Array.unsafe_racy_set_contended output (Int.to_int_exn keep_i) input_i));
    if Int.not_eq filter_len (Bigstring0.unsafe_get keep (length - 1))
    then
      Array.unsafe_racy_set_contended
        output
        (Int.to_int_exn filter_len - 1)
        (Array.unsafe_racy_get_contended input (length - 1) |> Obj.magic_uncontended);
    Array.freeze output
  ;;

  let filteri parallel input ~f =
    let length = Array.length input in
    if length = 0
    then input
    else if length <= Ints.I8.max_int
    then (filteri_gen [@mode m]) (module Ints.I8) ~kind:Int8 parallel input ~f
    else if length <= Ints.I16.max_int
    then (filteri_gen [@mode m]) (module Ints.I16) ~kind:Int16 parallel input ~f
    else if length <= Ints.I32.max_int
    then (filteri_gen [@mode m]) (module Ints.I32) ~kind:Int32 parallel input ~f
    else (filteri_gen [@mode m]) (module Ints.I64) ~kind:Int64 parallel input ~f
  ;;

  let[@inline] filter parallel input ~f =
    (filteri [@mode m]) parallel input ~f:(fun parallel _ a -> f parallel a) [@nontail]
  ;;]
end
[@@kind k = (value, value_or_null mod separable)]

module Make_filter_map (Array : sig
  @@ portable
    type ('a : value_or_null mod portable separable) t

    [%%template:
    [@@@mode.default m = (uncontended, shared)]

    val mapi
      : ('b : value_or_null mod portable separable).
      Parallel_kernel.t @ local
      -> 'a t @ m
      -> f:(Parallel_kernel.t @ local -> int -> 'a @ m -> 'b) @ shareable
      -> 'b t

    val filter
      : ('a : value_or_null mod portable separable).
      Parallel_kernel.t @ local
      -> 'a t @ m
      -> f:(Parallel_kernel.t @ local -> 'a @ m -> bool) @ shareable
      -> 'a t @ m]
  end) =
struct
  (* There must be no [Null] elements. *)
  external unsafe_unwrap : 'a or_null Array.t -> 'a Array.t @@ portable = "%obj_magic"

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let filter_mapi parallel input ~f =
    let output = (Array.mapi [@mode m]) parallel input ~f in
    Array.filter parallel output ~f:(fun _ a -> Or_null.is_this a) |> unsafe_unwrap
  ;;

  let[@inline] filter_map parallel input ~f =
    (filter_mapi [@mode m]) parallel input ~f:(fun parallel _ a -> f parallel a)
    [@nontail]
  ;;]
end

module%template Make_slice (Array : sig
  @@ portable
    type ('a : value mod portable) t : k with 'a

    val length : 'a t @ shared -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  end) =
struct
  type 'a t =
    { array : 'a Array.t @@ contended global
    ; start : int
    ; stop : int
    }

  let length { start; stop; _ } = stop - start

  let check_pivots pivots ~len =
    Iarray.fold pivots ~init:0 ~f:(fun acc pivot ->
      if pivot < 0 || pivot > len then invalid_arg "index out of bounds";
      if pivot < acc then invalid_arg "pivots must be non-decreasing";
      pivot)
    |> (ignore : int -> unit)
  ;;

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

  let[@inline] get { array; start; stop } i =
    if i < 0 || i >= stop - start then invalid_arg "index out of bounds";
    Array.unsafe_racy_get_contended array (start + i) |> Obj.magic_uncontended
  ;;

  let[@inline] unsafe_get { array; start; _ } i =
    Array.unsafe_racy_get_contended array (start + i) |> Obj.magic_uncontended
  ;;

  let[@inline] extract t i f = f ((get [@mode m]) t i)
  let[@inline] unsafe_extract t i f = f ((unsafe_get [@mode m]) t i)

  let fork_join2 parallel ?pivot { array; start; stop } f1 f2 =
    let len = stop - start in
    let pivot = Option.value pivot ~default:(len / 2) in
    if pivot < 0 || pivot > len then invalid_arg "index out of bounds";
    Parallel_kernel.fork_join2
      parallel
      (fun parallel -> f1 parallel { array; start; stop = start + pivot })
      (fun parallel -> f2 parallel { array; start = start + pivot; stop }) [@nontail]
  ;;

  let fori parallel ~pivots { array; start; stop } ~f =
    let len = stop - start in
    check_pivots pivots ~len;
    let num_pivots = Iarray.length pivots in
    Parallel_kernel.for_ parallel ~start:0 ~stop:(num_pivots + 1) ~f:(fun parallel i ->
      let start' = if i = 0 then start else start + Iarray.get pivots (i - 1) in
      let stop' = if i = num_pivots then stop else start + Iarray.get pivots i in
      f parallel i { array; start = start'; stop = stop' })
    [@nontail]
  ;;

  let[@inline] for_ parallel ~pivots slice ~f =
    fori parallel ~pivots slice ~f:(fun parallel _ subslice -> f parallel subslice)
    [@nontail]
  ;;
end
[@@kind k = (mutable_data, immutable_data)]

module Make_islice (Array : sig
  @@ portable
    type ('a : value mod portable) t : immutable_data with 'a

    val length : 'a t @ shared -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
  end) =
struct
  module%template Slice = Make_slice [@kind immutable_data] (Array)
end

module Make_slice (Array : sig
  @@ portable
    type ('a : value mod portable) t : mutable_data with 'a

    val length : 'a t @ shared -> int
    val unsafe_racy_get_contended : 'a t @ contended -> int -> 'a @ contended
    val unsafe_racy_set_contended : 'a t @ contended -> int -> 'a -> unit
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

    let[@inline] insert t i f = set t i (f ())
    let[@inline] unsafe_insert t i f = unsafe_set t i (f ())
  end
end

module Array = struct
  include Array

  type nonrec ('a : any) t = 'a t
  type ('a : any) mut = 'a t
  type 'a init = int

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external wrap : 'a t @ m -> ('a modality[@mode m]) t @ m @@ portable = "%identity"
  external unwrap : ('a modality[@mode m]) t @ m -> 'a t @ m @@ portable = "%identity"

  external of_array
    : ('a : value mod contended portable).
    'a array @ m -> 'a array @ m
    @@ portable
    = "%identity"

  external to_array
    : ('a : value mod contended portable).
    'a array @ m -> 'a array @ m
    @@ portable
    = "%identity"

  let[@inline] freeze mut = mut
  let[@inline] extract t i f = f ((get [@mode m]) t i)
  let[@inline] unsafe_extract t i f = f ((unsafe_get [@mode m]) t i)]

  let[@inline] insert t i f = set t i (f ())
  let[@inline] unsafe_insert t i f = unsafe_set t i (f ())
  let[@inline] to_length init = init
  let[@inline] create n a = create ~len:n a
  let[@inline] create_like _ ~len a = create len a
  let[@inline] empty () = [||]
  let[@inline] empty_like _ = empty ()

  include functor Make_slice
  include functor Make_init
  include functor Make_inplace
  include functor Make_reduce
  include functor Make_sort
  include functor Make_scan
  include functor Make_map [@kind value_or_null mod separable]
  include functor Make_filter [@kind value_or_null mod separable]
  include functor Make_filter_map
end

module Iarray = struct
  include Iarray

  type nonrec ('a : any) t = 'a t
  type ('a : any) mut = 'a array
  type 'a init = int

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external of_iarray
    : ('a : value mod contended portable).
    'a iarray @ m -> 'a iarray @ m
    @@ portable
    = "%identity"

  external to_iarray
    : ('a : value mod contended portable).
    'a iarray @ m -> 'a iarray @ m
    @@ portable
    = "%identity"

  let wrap = (Array.wrap [@mode m])
  let unwrap = (Array.unwrap [@mode m])

  let[@inline] copy t =
    (unsafe_to_array__promise_no_mutation [@mode m]) t |> (Array.copy [@mode m])
  ;;

  let freeze = (unsafe_of_array__promise_no_mutation [@mode m])
  let[@inline] extract t i f = f ((get [@mode m]) t i)
  let[@inline] unsafe_extract t i f = f ((unsafe_get [@mode m]) t i)]

  let[@inline] to_length init = init
  let[@inline] empty () = [::]
  let[@inline] empty_like _ = empty ()
  let create = Array.create
  let create_like = Array.create_like
  let sort_inplace = Array.sort_inplace
  let stable_sort_inplace = Array.stable_sort_inplace
  let scan_inplace = Array.scan_inplace
  let scan_inclusive_inplace = Array.scan_inclusive_inplace
  let unsafe_racy_set_contended = Array.unsafe_racy_set_contended

  include functor Make_islice
  include functor Make_init
  include functor Make_reduce
  include functor Make_sort
  include functor Make_scan
  include functor Make_map [@kind value_or_null mod separable]
  include functor Make_filter [@kind value_or_null mod separable]
  include functor Make_filter_map
end

module Vec = struct
  include Vec

  type nonrec 'a t = 'a t
  type 'a init = int
  type 'a mut = 'a t

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  external wrap : 'a t @ m -> ('a modality[@mode m]) t @ m @@ portable = "%identity"
  external unwrap : ('a modality[@mode m]) t @ m -> 'a t @ m @@ portable = "%identity"

  external of_vec
    : ('a : value mod contended portable).
    'a Vec.t @ m -> 'a Vec.t @ m
    @@ portable
    = "%identity"

  external to_vec
    : ('a : value mod contended portable).
    'a Vec.t @ m -> 'a Vec.t @ m
    @@ portable
    = "%identity"

  let[@inline] freeze mut = mut
  let[@inline] extract t i f = f ((get [@mode m]) t i)
  let[@inline] unsafe_extract t i f = f ((unsafe_get [@mode m]) t i)]

  let[@inline] insert t i f = set t i (f ())
  let[@inline] unsafe_insert t i f = unsafe_set t i (f ())
  let[@inline] to_length init = init
  let[@inline] create n a = init n ~f:(fun _ -> a) [@nontail]
  let[@inline] create_like _ ~len a = create len a
  let[@inline] empty () = Vec.create ()
  let[@inline] empty_like _ = empty ()

  include functor Make_slice
  include functor Make_init
  include functor Make_inplace
  include functor Make_reduce
  include functor Make_sort
  include functor Make_scan
  include functor Make_map
  include functor Make_filter
end

module Bigstring = struct
  include Bigstring0

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let get = get
  let unsafe_get = unsafe_get
  let[@inline] extract t i f = f (get t i)
  let[@inline] unsafe_extract t i f = f (unsafe_get t i)]

  let[@inline] insert t i f = set t i (f ())
  let[@inline] unsafe_insert t i f = unsafe_set t i (f ())

  include functor Make_slice
  include functor Make_reduce
  include functor Make_sort
  include functor Make_filter

  let%template of_slice (slice : 'a Slice.t) : 'a t @ m =
    (* An uncontended/shared slice indicates uncontended/shared access to the protected
       index range. *)
    let t = Obj.magic_uncontended slice.array in
    (sub_shared [@mode m]) t ~pos:slice.start ~len:(slice.stop - slice.start)
  [@@mode m = (uncontended, shared)]
  ;;
end

module Bigarray = struct
  module Kind = struct
    type ('a, 'k) t = ('a, 'k) Bigarray.kind =
      | Float32 : (float, Bigarray.float32_elt) t
      | Float64 : (float, Bigarray.float64_elt) t
      | Int8_signed : (int, Bigarray.int8_signed_elt) t
      | Int8_unsigned : (int, Bigarray.int8_unsigned_elt) t
      | Int16_signed : (int, Bigarray.int16_signed_elt) t
      | Int16_unsigned : (int, Bigarray.int16_unsigned_elt) t
      | Int32 : (int32, Bigarray.int32_elt) t
      | Int64 : (int64, Bigarray.int64_elt) t
      | Int : (int, Bigarray.int_elt) t
      | Nativeint : (nativeint, Bigarray.nativeint_elt) t
      | Complex32 : (Stdlib.Complex.t, Bigarray.complex32_elt) t
      | Complex64 : (Stdlib.Complex.t, Bigarray.complex64_elt) t
      | Char : (char, Bigarray.int8_unsigned_elt) t
      | Float16 : (float, Bigarray.float16_elt) t
  end

  module Layout = struct
    type 'a t = 'a Bigarray.layout =
      | C_layout : Bigarray.c_layout t
      | Fortran_layout : Bigarray.fortran_layout t
  end

  module Spec = struct
    type 'a t = T : ('a, _) Kind.t * _ Layout.t -> 'a t
  end

  module Array = struct
    include Bigarray.Array1

    external unsafe_racy_get_contended
      :  (('a, 'b, 'c) t[@local_opt]) @ contended
      -> int
      -> ('a[@local_opt]) @ contended
      @@ portable
      = "%caml_ba_unsafe_ref_1"

    external unsafe_racy_set_contended
      :  (('a, 'b, 'c) t[@local_opt]) @ contended
      -> int
      -> ('a[@local_opt])
      -> unit
      @@ portable
      = "%caml_ba_unsafe_set_1"
  end

  type 'a t = T : ('a, _, _) Array.t -> 'a t [@@unboxed]
  type 'a mut = 'a t
  type 'a init = 'a Spec.t * int

  let[@inline] unsafe_racy_get_contended (T t) i = Array.unsafe_racy_get_contended t i
  let[@inline] unsafe_racy_set_contended (T t) i a = Array.unsafe_racy_set_contended t i a

  let create (Spec.T (kind, layout), n) a =
    let t = T (Array.create kind layout n) in
    unsafe_racy_set_contended t 0 a;
    t
  ;;

  let[@inline] create_like (T t) ~len a = create (T (Array.kind t, Array.layout t), len) a
  let[@inline] empty_like (Spec.T (kind, layout), _) = T (Array.create kind layout 0)
  let[@inline] to_length (_, n) = n
  let[@inline] kind kind layout = Spec.T (kind, layout)
  let[@inline] length (T t) = Array.dim t

  [%%template
  [@@@mode.default m = (uncontended, shared)]

  let copy (T src) =
    let dst = Array.create (Array.kind src) (Array.layout src) (Array.dim src) in
    Array.blit src dst;
    T dst
  ;;

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

  let[@inline] of_bigarray t = T t
  let[@inline] freeze t = t
  let[@inline] get (T t) i = Array.get t i
  let[@inline] unsafe_get (T t) i = Array.unsafe_get t i
  let[@inline] extract t i f = f (get t i)
  let[@inline] unsafe_extract t i f = f (unsafe_get t i)]

  let[@inline] set (T t) i a = Array.set t i a
  let[@inline] unsafe_set (T t) i a = Array.unsafe_set t i a
  let[@inline] insert t i f = set t i (f ())
  let[@inline] unsafe_insert t i f = unsafe_set t i (f ())

  include functor Make_inplace
  include functor Make_init
  include functor Make_scan
  include functor Make_slice
  include functor Make_reduce
  include functor Make_sort
  include functor Make_filter

  let%template of_slice (slice : 'a Slice.t) : 'a t @ m =
    (* An uncontended/shared slice indicates uncontended/shared access to the protected
       index range. *)
    let (T t) = Obj.magic_uncontended slice.array in
    T (Array.sub t slice.start (slice.stop - slice.start))
  [@@mode m = (uncontended, shared)]
  ;;
end
