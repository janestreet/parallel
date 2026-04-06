open! Base
open Unboxed_datatypes
include Parallel_map_intf.Definitions
module Parallel = Parallel_kernel
module Sequence = Parallel_sequence

open struct
  (* Helper definitions, not for export. *)

  module Tree = Map.Tree
  module Enum = Tree.Enum

  type ('key, 'cmp : value mod portable) comparator = ('key, 'cmp) Comparator.t

  type ('key : value mod contended portable
       , 'data : value mod contended portable
       , 'cmp)
       tree =
    ('key, 'data, 'cmp) Tree.t

  type ('key : value mod contended portable
       , 'data : value mod contended portable
       , 'cmp
       , 'dir)
       enum =
    ('key, 'data, 'cmp, 'dir) Enum.t

  type ('a : value mod contended portable
       , 'b : value mod contended portable)
       merge_element :
       value mod contended portable =
    ('a, 'b) Map.Merge_element.t

  let empty : (_, _, _) tree = Tree.empty_without_value_restriction
end

module Tree = struct
  let to_sequence (tree : (_, _, _) tree) = exclave_
    Sequence.With_length.unfold
      ~init:(Enum.of_tree tree)
      ~next:(fun _ enum ->
        let nonempty = Or_null.value_exn enum in
        #((Enum.key nonempty, Enum.data nonempty), Enum.next nonempty))
      ~split_at:(fun _ enum ~n -> Enum.split_n enum n)
      ~length:Enum.length
  ;;

  let traverse par (tree : (_, _, _) tree) ~on_empty ~on_data ~on_leaf ~on_node =
    let rec loop par (tree : (_, _, _) tree) =
      match tree with
      | Empty -> on_empty ()
      | Leaf { key; data } -> on_leaf par ~key (on_data par ~key ~data)
      | Node { key; data; left; right; weight = _ } ->
        let #(left, mid, right) =
          Parallel.fork_join3
            par
            (fun par -> loop par left)
            (fun par -> on_data par ~key ~data)
            (fun par -> loop par right)
        in
        on_node par ~key mid left right
    in
    loop par tree
  ;;

  let fold par (tree : (_, _, _) tree) ~init ~f ~combine =
    Parallel.fold
      par
      ~state:(Enum.of_tree tree)
      ~init
      ~next:(fun par acc -> function
        | Null -> (Option_u.none [@kind value_or_null & value_or_null]) ()
        | This enum ->
          (Option_u.some [@kind value_or_null & value_or_null])
            #(f par ~key:(Enum.key enum) ~data:(Enum.data enum) acc, Enum.next enum))
      ~stop:(fun _ acc -> acc)
      ~fork:(fun _ enum ->
        match Enum.length enum with
        | 0 | 1 -> (Option_u.none [@kind value_or_null & value_or_null]) ()
        | n ->
          let #(prefix, suffix) = Enum.split_n enum (n / 2) in
          (Option_u.some [@kind value_or_null & value_or_null]) #(prefix, suffix))
      ~join:combine [@nontail]
  ;;

  let iter par tree ~f =
    fold
      par
      tree
      ~init:(fun () -> ())
      ~f:(fun par ~key ~data () -> f par ~key ~data)
      ~combine:(fun _ () () -> ()) [@nontail]
  ;;

  let map par (tree : (_, _, _) tree) ~f =
    traverse
      par
      tree
      ~on_empty:(fun () -> empty)
      ~on_data:f
      ~on_leaf:(fun _ ~key data -> Tree.Expert.singleton key data)
      ~on_node:(fun _ ~key data left right ->
        (* Correct because we preserve the keys and balance of the original. *)
        Tree.Expert.create_assuming_balanced_unchecked left key data right)
  ;;

  let filter par (tree : (_, _, _) tree) ~f =
    let rec loop par (tree : (_, _, _) tree) =
      match tree with
      | Empty -> empty
      | Leaf { key; data } -> if f par ~key ~data then tree else empty
      | Node { key; data; left = original_left; right = original_right; weight = _ } ->
        let #(filtered_left, bool, filtered_right) =
          Parallel.fork_join3
            par
            (fun par -> loop par original_left)
            (fun par -> f par ~key ~data)
            (fun par -> loop par original_right)
        in
        (match bool with
         | false ->
           (* Correct because we preserve (a subset of) the original key order. *)
           Tree.Expert.concat_and_rebalance_unchecked filtered_left filtered_right
         | true ->
           if phys_equal filtered_left original_left
              && phys_equal filtered_right original_right
           then tree
           else
             (* Correct because we preserve (a subset of) the original key order. *)
             Tree.Expert.create_and_rebalance_unchecked
               filtered_left
               key
               data
               filtered_right)
    in
    loop par tree
  ;;

  let filter_map par (tree : (_, _, _) tree) ~f =
    traverse
      par
      tree
      ~on_empty:(fun () -> empty)
      ~on_data:f
      ~on_leaf:(fun _ ~key data ->
        match data with
        | Some data -> Tree.Expert.singleton key data
        | None -> empty)
      ~on_node:(fun _ ~key data left right ->
        match data with
        | Some data ->
          (* Correct because we preserve (a subset of) the original key order. *)
          Tree.Expert.create_and_rebalance_unchecked left key data right
        | None ->
          (* Correct because we preserve (a subset of) the original key order. *)
          Tree.Expert.concat_and_rebalance_unchecked left right)
  ;;

  let partition_tf par (tree : (_, _, _) tree) ~f =
    let rec loop par (tree : (_, _, _) tree) =
      match tree with
      | Empty -> empty, empty
      | Leaf { key; data } -> if f par ~key ~data then tree, empty else empty, tree
      | Node { key; data; left = original_left; right = original_right; weight = _ } ->
        let #((left_t, left_f), bool, (right_t, right_f)) =
          Parallel.fork_join3
            par
            (fun par -> loop par original_left)
            (fun par -> f par ~key ~data)
            (fun par -> loop par original_right)
        in
        (match bool with
         | true ->
           if phys_equal left_t original_left && phys_equal right_t original_right
           then tree, empty
           else
             (* Correct because we preserve (a subset of) the original key order. *)
             ( Tree.Expert.create_and_rebalance_unchecked left_t key data right_t
             , Tree.Expert.concat_and_rebalance_unchecked left_f right_f )
         | false ->
           if phys_equal left_f original_left && phys_equal right_f original_right
           then empty, tree
           else
             (* Correct because we preserve (a subset of) the original key order. *)
             ( Tree.Expert.concat_and_rebalance_unchecked left_t right_t
             , Tree.Expert.create_and_rebalance_unchecked left_f key data right_f ))
    in
    loop par tree
  ;;

  let partition_map par (tree : (_, _, _) tree) ~f =
    let open Either.Export in
    traverse
      par
      tree
      ~on_empty:(fun () -> empty, empty)
      ~on_data:f
      ~on_leaf:(fun _ ~key data ->
        match data with
        | First data -> Tree.Expert.singleton key data, empty
        | Second data -> empty, Tree.Expert.singleton key data)
      ~on_node:(fun _ ~key data (left1, left2) (right1, right2) ->
        match data with
        | First data ->
          (* Correct because we preserve (a subset of) the original key order. *)
          ( Tree.Expert.create_and_rebalance_unchecked left1 key data right1
          , Tree.Expert.concat_and_rebalance_unchecked left2 right2 )
        | Second data ->
          (* Correct because we preserve (a subset of) the original key order. *)
          ( Tree.Expert.concat_and_rebalance_unchecked left1 right1
          , Tree.Expert.create_and_rebalance_unchecked left2 key data right2 ))
  ;;

  let to_sequence2
    ~(comparator : (_, _) comparator)
    (tree1 : (_, _, _) tree)
    (tree2 : (_, _, _) tree)
    = exclave_
    let init = Enum.of_tree tree1, Enum.of_tree tree2 in
    let next
      :  _ @ local -> (_, _, _, _) enum * (_, _, _, _) enum
      -> (#((_ * (_, _) merge_element) * ((_, _, _, _) enum * (_, _, _, _) enum))
            Option_u.t
         [@kind value_or_null & value_or_null])
      =
      fun _ (enum1, enum2) ->
      match enum1, enum2 with
      | Null, Null -> (Option_u.none [@kind value_or_null & value_or_null]) ()
      | This enum1, Null ->
        (Option_u.some [@kind value_or_null & value_or_null])
          #((Enum.key enum1, `Left (Enum.data enum1)), (Enum.next enum1, enum2))
      | Null, This enum2 ->
        (Option_u.some [@kind value_or_null & value_or_null])
          #((Enum.key enum2, `Right (Enum.data enum2)), (enum1, Enum.next enum2))
      | This enum1, This enum2 ->
        (* Trying to force specialization so we only dispatch on key order once. *)
        let f which =
          let k = (Enum.which_key [@inlined]) enum1 enum2 ~which in
          let v = (Enum.which_merge_element [@inlined]) enum1 enum2 ~which in
          let #(enum1, enum2) = (Enum.next2 [@inlined]) enum1 enum2 ~which in
          (Option_u.some [@kind value_or_null & value_or_null]) #((k, v), (enum1, enum2))
        in
        (match
           (Enum.which [@inlined])
             enum1
             enum2
             ~compare_key:(Comparator.compare comparator)
         with
         | Left as which -> (f [@inlined]) which
         | Right as which -> (f [@inlined]) which
         | Both as which -> (f [@inlined]) which)
    in
    let split _ (enum1, enum2) =
      match Enum.split2 enum1 enum2 ~compare_key:(Comparator.compare comparator) with
      | Null -> (Option_u.none [@kind value_or_null & value_or_null]) ()
      | This (prefix1, suffix1, prefix2, suffix2) ->
        (Option_u.some [@kind value_or_null & value_or_null])
          #((prefix1, suffix1), (prefix2, suffix2))
    in
    Sequence.unfold ~init ~next ~split
  ;;

  let merge_filter_map
    ~(comparator : (_, _) comparator)
    par
    (tree1 : (_, _, _) tree)
    (tree2 : (_, _, _) tree)
    ~(f :
        (_ @ local
         -> key:(_ : value mod portable)
         -> _
         -> (_ : value mod portable) option)
        @ shareable)
    =
    Sequence.fold
      par
      (to_sequence2 ~comparator tree1 tree2)
      ~init:(fun () -> empty)
      ~f:(fun par acc (key, either_or_both) ->
        match f par ~key either_or_both with
        | None -> acc
        | Some data ->
          (* Correct because we preserve (a subset of) the original key order. *)
          Tree.Expert.concat_and_rebalance_unchecked acc (Tree.Expert.singleton key data))
      ~combine:(fun _ tree1 tree2 ->
        (* Correct because we preserve (a subset of) the original key order. *)
        Tree.Expert.concat_and_rebalance_unchecked tree1 tree2) [@nontail]
  ;;
end

let to_sequence map = exclave_ Tree.to_sequence (Map.to_tree map)

let traverse par map ~on_empty ~on_data ~on_leaf ~on_node =
  Tree.traverse par (Map.to_tree map) ~on_empty ~on_data ~on_leaf ~on_node
;;

let fold par map ~init ~f ~combine = Tree.fold par (Map.to_tree map) ~init ~f ~combine
let iter par map ~f = Tree.iter par (Map.to_tree map) ~f

let map par map ~f =
  Tree.map par (Map.to_tree map) ~f
  |> Map.Using_comparator.of_tree ~comparator:(Map.comparator map)
;;

let filter par map ~f =
  Tree.filter par (Map.to_tree map) ~f
  |> Map.Using_comparator.of_tree ~comparator:(Map.comparator map)
;;

let filter_map par map ~f =
  Tree.filter_map par (Map.to_tree map) ~f
  |> Map.Using_comparator.of_tree ~comparator:(Map.comparator map)
;;

let partition_tf par map ~f =
  let comparator = Map.comparator map in
  let tree1, tree2 = Tree.partition_tf par (Map.to_tree map) ~f in
  ( Map.Using_comparator.of_tree ~comparator tree1
  , Map.Using_comparator.of_tree ~comparator tree2 )
;;

let partition_map par map ~f =
  let comparator = Map.comparator map in
  let tree1, tree2 = Tree.partition_map par (Map.to_tree map) ~f in
  ( Map.Using_comparator.of_tree ~comparator tree1
  , Map.Using_comparator.of_tree ~comparator tree2 )
;;

let to_sequence2 map1 map2 = exclave_
  let comparator = Map.comparator map1 in
  let tree1 = Map.to_tree map1 in
  let tree2 = Map.to_tree map2 in
  Tree.to_sequence2 ~comparator tree1 tree2
;;

let merge_filter_map par map1 map2 ~f =
  let comparator = Map.comparator map1 in
  let tree1 = Map.to_tree map1 in
  let tree2 = Map.to_tree map2 in
  Tree.merge_filter_map ~comparator par tree1 tree2 ~f
  |> Map.Using_comparator.of_tree ~comparator
;;
