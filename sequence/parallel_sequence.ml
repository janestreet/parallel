open! Base
open! Import
include Parallel_sequence_intf

let failwith s : _ Pair_or_null.t =
  match failwith s with
  | (_ : Nothing.t) -> .
;;

type ('s : value mod contended portable unyielding
     , 'a : value mod portable unyielding)
     unknown =
  { current : 's @@ global
  ; next : Parallel_kernel.t @ local -> 's -> ('a, 's) Pair_or_null.t
    @@ global portable unyielding
  ; split : Parallel_kernel.t @ local -> 's -> ('s, 's) Pair_or_null.t
    @@ global portable unyielding
  }

type ('s : value mod contended portable unyielding
     , 'a : value mod portable unyielding)
     known =
  { current : 's @@ global
  ; next : Parallel_kernel.t @ local -> 's -> ('a, 's) Pair_or_null.t
    @@ global portable unyielding
  ; split_at : Parallel_kernel.t @ local -> 's -> n:int -> ('s, 's) Pair_or_null.t
    @@ global portable unyielding
  ; length : 's -> int @@ global portable unyielding
  }

type%fuelproof (_ : value mod portable unyielding
               , _)
               seq :
               value mod contended portable unyielding =
  | Unknown : (_, 'a) unknown -> ('a, [> `Unknown ]) seq
  | Known : (_, 'a) known -> ('a, [> `Known ]) seq

let parallel_fold parallel ~f ~init ~state ~next ~split ~combine =
  Parallel_kernel.fold
    parallel
    ~init
    ~state
    ~next:(fun parallel acc state ->
      match%optional_u.Pair_or_null next parallel state with
      | Some a_state ->
        let #(a, state) = a_state in
        Pair_or_null.some (f parallel acc a) state
      | None -> Pair_or_null.none ())
    ~stop:(fun _ acc -> acc)
    ~fork:split
    ~join:combine [@nontail]
;;

module With_length = struct
  type 'a t = ('a, [ `Known ]) seq

  let[@inline always] globalize : _ t @ local -> _ t = function
    | Known { current; next; split_at; length } ->
      Known { current; next; split_at; length }
  ;;

  let length : _ t @ local -> int = function
    | Known { current; length; _ } -> length current
  ;;

  let[@inline always] split_middle { split_at; length; _ } =
    ();
    fun parallel current -> split_at parallel current ~n:(length current / 2)
  ;;

  let unfold ~init ~next ~split_at ~length = exclave_
    Known { current = init; next; split_at; length }
  ;;

  let empty =
    Known
      { current = ()
      ; next = (fun _ () -> Pair_or_null.none ())
      ; split_at = (fun _ () ~n:_ -> Pair_or_null.none ())
      ; length = (fun () -> 0)
      }
  ;;

  let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i
    = exclave_
    if stride = 0 then invalid_arg "Parallel_sequence.range";
    let is_downto = stride < 0 in
    let sign = Bool.select is_downto (-1) 1 in
    let start, stop =
      match start, stop with
      | `inclusive, `inclusive -> start_i, stop_i + sign
      | `inclusive, `exclusive -> start_i, stop_i
      | `exclusive, `inclusive -> start_i + stride, stop_i + sign
      | `exclusive, `exclusive -> start_i + stride, stop_i
    in
    if (stride < 0 && start < stop) || (stride > 0 && stop < start)
    then empty
    else (
      let length (~start, ~stop) = (stop - start + stride - sign) / stride in
      Known
        { current = ((~start, ~stop) : start:int * stop:int)
        ; length
        ; next =
            (fun _ (~start, ~stop) ->
              let low = Bool.select is_downto stop start in
              let high = Bool.select is_downto start stop in
              if low < high
              then Pair_or_null.some start (~start:(start + stride), ~stop)
              else Pair_or_null.none ())
        ; split_at =
            (fun _ (~start, ~stop) ~n ->
              if n < 1 || length (~start, ~stop) - n < 1
              then Pair_or_null.none ()
              else (
                let pivot = start + (n * stride) in
                Pair_or_null.some (~start, ~stop:pivot) (~start:pivot, ~stop)))
        })
  ;;

  let map' (t : _ t) ~f = exclave_
    match t with
    | Known ({ next; _ } as known) ->
      let next parallel current =
        match%optional_u.Pair_or_null next parallel current with
        | None -> Pair_or_null.none ()
        | Some a_current ->
          let #(a, current) = a_current in
          Pair_or_null.some (f parallel a) current
      in
      Known { known with next }
  ;;

  let map t ~f = exclave_ map' t ~f:(fun _ a -> f a) [@nontail]

  let filter_map' (t : _ t) ~f = exclave_
    match t with
    | Known ({ current; next; _ } as known) ->
      let[@loop] rec next' parallel current =
        match%optional_u.Pair_or_null next parallel current with
        | None -> Pair_or_null.none ()
        | Some a_current ->
          let #(a, current) = a_current in
          (match f parallel a with
           | None -> next' parallel current
           | Some a -> Pair_or_null.some a current)
      in
      Unknown { current; next = next'; split = split_middle known }
  ;;

  let init' n ~f = exclave_
    let ints = range 0 n in
    map' ints ~f
  ;;

  let init n ~f = exclave_ init' n ~f:(fun _ i -> f i) [@nontail]

  let of_iarray iarray = exclave_
    Known
      { current =
          ((iarray, ~start:0, ~stop:(Iarray.length iarray))
           : (_ : value mod contended portable unyielding) iarray * start:int * stop:int)
      ; length = (fun (_, ~start, ~stop) -> stop - start)
      ; next =
          (fun _ (iarray, ~start, ~stop) ->
            if start < stop
            then
              Pair_or_null.some
                (Iarray.unsafe_get iarray start)
                (iarray, ~start:(start + 1), ~stop)
            else Pair_or_null.none ())
      ; split_at =
          (fun _ (iarray, ~start, ~stop) ~n ->
            if n < 1 || stop - start - n < 1
            then Pair_or_null.none ()
            else (
              let pivot = start + n in
              Pair_or_null.some (iarray, ~start, ~stop:pivot) (iarray, ~start:pivot, ~stop)))
      }
  ;;

  let zip_exn (t0 : _ t) (t1 : _ t) : _ t = exclave_
    match t0, t1 with
    | ( Known (type (s0 : value mod contended portable unyielding))
          ({ current = current0; next = next0; split_at = split_at0; length = length0 } :
            (s0, _) known)
      , Known (type (s1 : value mod contended portable unyielding))
          ({ current = current1; next = next1; split_at = split_at1; length = length1 } :
            (s1, _) known) ) ->
      if length0 current0 <> length1 current1
      then invalid_arg "Parallel_sequence.With_length.zip_exn";
      Known
        { current = ((current0, current1) : s0 * s1)
        ; next =
            (fun parallel (current0, current1) ->
              match%optional_u.Pair_or_null
                next0 parallel current0, next1 parallel current1
              with
              | None, None -> Pair_or_null.none ()
              | Some res0, Some res1 ->
                let #(a0, current0) = res0 in
                let #(a1, current1) = res1 in
                Pair_or_null.some (a0, a1) (current0, current1)
              | None, _ ->
                failwith "Parallel_sequence.With_length.zip_exn got mismatched next!"
              | _, None ->
                failwith "Parallel_sequence.With_length.zip_exn got mismatched next!")
        ; split_at =
            (fun parallel (current0, current1) ~n ->
              match%optional_u.Pair_or_null
                split_at0 parallel current0 ~n, split_at1 parallel current1 ~n
              with
              | Some current0, Some current1 ->
                let #(current00, current01) = current0 in
                let #(current10, current11) = current1 in
                Pair_or_null.some (current00, current10) (current01, current11)
              | _ -> Pair_or_null.none ())
        ; length = (fun (current0, _) -> length0 current0)
        }
  ;;

  let indexed t = exclave_ zip_exn (range 0 (length t)) t

  let mapi' t ~f = exclave_
    map' (indexed t) ~f:(fun parallel (i, a) -> f parallel i a) [@nontail]
  ;;

  let mapi t ~f = exclave_ mapi' t ~f:(fun _ i a -> f i a) [@nontail]

  module Append = struct
    type ('s0, 's1) t =
      | Left of 's0
      | Right of 's1
      | Both of 's0 * 's1

    let create s0 s1 = Both (s0, s1)

    let next_left ~next0 parallel s0 =
      match%optional_u.Pair_or_null next0 parallel s0 with
      | Some a_current ->
        let #(a, current) = a_current in
        Pair_or_null.some a (Left current)
      | None -> Pair_or_null.none ()
    ;;

    let next_right ~next1 parallel s1 =
      match%optional_u.Pair_or_null next1 parallel s1 with
      | Some a_current ->
        let #(a, current) = a_current in
        Pair_or_null.some a (Right current)
      | None -> Pair_or_null.none ()
    ;;

    let next_append ~next0 ~next1 =
      ();
      fun parallel t ->
        match t with
        | Left s0 -> next_left ~next0 parallel s0
        | Right s1 -> next_right ~next1 parallel s1
        | Both (s0, s1) ->
          (match%optional_u.Pair_or_null next0 parallel s0 with
           | Some a_s0 ->
             let #(a, s0) = a_s0 in
             Pair_or_null.some a (Both (s0, s1))
           | None -> next_right ~next1 parallel s1)
    ;;

    let length_append ~length0 ~length1 =
      ();
      fun t ->
        match t with
        | Left s0 -> length0 s0
        | Right s1 -> length1 s1
        | Both (s0, s1) -> length0 s0 + length1 s1
    ;;

    let split_at_append ~length0 ~split_at0 ~split_at1 =
      ();
      fun parallel t ~n ->
        match t with
        | Both (s0, s1) ->
          let len0 = length0 s0 in
          if n = len0
          then Pair_or_null.some (Left s0) (Right s1)
          else if n < len0
          then (
            match%optional_u.Pair_or_null split_at0 parallel s0 ~n with
            | None -> Pair_or_null.none ()
            | Some s00_s01 ->
              let #(s00, s01) = s00_s01 in
              Pair_or_null.some (Left s00) (Both (s01, s1)))
          else (
            match%optional_u.Pair_or_null split_at1 parallel s1 ~n:(n - len0) with
            | None -> Pair_or_null.none ()
            | Some s10_s11 ->
              let #(s10, s11) = s10_s11 in
              Pair_or_null.some (Both (s0, s10)) (Right s11))
        | Left s0 ->
          (match%optional_u.Pair_or_null split_at0 parallel s0 ~n with
           | None -> Pair_or_null.none ()
           | Some s00_s01 ->
             let #(s00, s01) = s00_s01 in
             Pair_or_null.some (Left s00) (Left s01))
        | Right s1 ->
          (match%optional_u.Pair_or_null split_at1 parallel s1 ~n with
           | None -> Pair_or_null.none ()
           | Some s10_s11 ->
             let #(s10, s11) = s10_s11 in
             Pair_or_null.some (Right s10) (Right s11))
    ;;
  end

  let append (Known seq0 : _ t) (Known seq1 : _ t) = exclave_
    Known
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:seq1.next
      ; split_at =
          Append.split_at_append
            ~length0:seq0.length
            ~split_at0:seq0.split_at
            ~split_at1:seq1.split_at
      ; length = Append.length_append ~length0:seq0.length ~length1:seq1.length
      }
  ;;

  module Product = struct
    type ('s0 : value mod contended portable unyielding
         , 's1 : value mod contended portable unyielding
         , 'a : value mod contended portable unyielding)
         t :
         value mod contended portable unyielding =
      | One of 'a * 's1
      | Prod of 's0 * 's1
      | Consl of 'a * 's1 * ('s0, 's1, 'a) t
      | Consr of ('s0, 's1, 'a) t * 'a * 's1

    let create s0 s1 = Prod (s0, s1)

    let rec length_prod ~length0 ~length1 t =
      match t with
      | One (_, s1) -> length1 s1
      | Prod (s0, s1) -> length0 s0 * length1 s1
      | Consl (_, s1, t) -> length1 s1 + length_prod ~length0 ~length1 t
      | Consr (t, _, s1) -> length1 s1 + length_prod ~length0 ~length1 t
    ;;

    let length_product ~length0 ~length1 =
      ();
      fun t -> length_prod ~length0 ~length1 t
    ;;

    let rec next_prod ~next0 ~next1 parallel t =
      match t with
      | One (a, s1) ->
        (match%optional_u.Pair_or_null next1 parallel s1 with
         | Some b_s1 ->
           let #(b, s1) = b_s1 in
           Pair_or_null.some (a, b) (One (a, s1))
         | None -> Pair_or_null.none ())
      | Prod (s0, s1) ->
        (match%optional_u.Pair_or_null next0 parallel s0 with
         | Some a_s0 ->
           let #(a, s0) = a_s0 in
           next_consl ~next0 ~next1 parallel a s1 (Prod (s0, s1))
         | None -> Pair_or_null.none ())
      | Consl (a, s1, t) -> next_consl ~next0 ~next1 parallel a s1 t
      | Consr (t, a, s1) -> next_consr ~next0 ~next1 parallel t a s1

    and next_consl ~next0 ~next1 parallel a s1 t =
      match%optional_u.Pair_or_null next1 parallel s1 with
      | Some b_s1 ->
        let #(b, s1) = b_s1 in
        Pair_or_null.some (a, b) (Consl (a, s1, t))
      | None -> next_prod ~next0 ~next1 parallel t

    and next_consr ~next0 ~next1 parallel t a s1 =
      match%optional_u.Pair_or_null next_prod ~next0 ~next1 parallel t with
      | Some ab_t ->
        let #(ab, t) = ab_t in
        Pair_or_null.some ab (Consr (t, a, s1))
      | None ->
        (match%optional_u.Pair_or_null next1 parallel s1 with
         | Some b_s1 ->
           let #(b, s1) = b_s1 in
           Pair_or_null.some (a, b) (Consr (t, a, s1))
         | None -> Pair_or_null.none ())
    ;;

    let next_product ~next0 ~next1 =
      ();
      fun parallel t -> next_prod ~next0 ~next1 parallel t
    ;;

    let rec split_at_prod
      ~next0
      ~next1
      ~length0
      ~length1
      ~split_at0
      ~split_at1
      parallel
      t
      ~n
      =
      match t with
      | One (a, s1) ->
        (match%optional_u.Pair_or_null split_at1 parallel s1 ~n with
         | Some s1 ->
           let #(s10, s11) = s1 in
           Pair_or_null.some (One (a, s10)) (One (a, s11))
         | None -> Pair_or_null.none ())
      | Consl (a, s1, t) ->
        let len = length1 s1 in
        (match Ordering.of_int (compare n len) with
         | Equal -> Pair_or_null.some (One (a, s1)) t
         | Less ->
           (match%optional_u.Pair_or_null split_at1 parallel s1 ~n with
            | Some s1 ->
              let #(s10, s11) = s1 in
              Pair_or_null.some (One (a, s10)) (Consl (a, s11, t))
            | None -> Pair_or_null.none ())
         | Greater ->
           (match%optional_u.Pair_or_null
              split_at_prod
                ~next0
                ~next1
                ~length0
                ~length1
                ~split_at0
                ~split_at1
                parallel
                t
                ~n:(n - len)
            with
            | Some t ->
              let #(t0, t1) = t in
              Pair_or_null.some (Consl (a, s1, t0)) t1
            | None -> Pair_or_null.none ()))
      | Consr (t, a, s1) ->
        let len = length_product ~length0 ~length1 t in
        (match Ordering.of_int (compare n len) with
         | Equal -> Pair_or_null.some t (One (a, s1))
         | Less ->
           (match%optional_u.Pair_or_null
              split_at_prod
                ~next0
                ~next1
                ~length0
                ~length1
                ~split_at0
                ~split_at1
                parallel
                t
                ~n
            with
            | Some t ->
              let #(t0, t1) = t in
              Pair_or_null.some t0 (Consr (t1, a, s1))
            | None -> Pair_or_null.none ())
         | Greater ->
           (match%optional_u.Pair_or_null split_at1 parallel s1 ~n:(n - len) with
            | Some s1 ->
              let #(s10, s11) = s1 in
              Pair_or_null.some (Consr (t, a, s10)) (One (a, s11))
            | None -> Pair_or_null.none ()))
      | Prod (outer, inner) ->
        let len_outer = length0 outer in
        let len_inner = length1 inner in
        let len = len_outer * len_inner in
        if n < 1 || len <= n
        then Pair_or_null.none ()
        else if n % len_inner = 0
        then (
          let n_outer = n / len_inner in
          match%optional_u.Pair_or_null split_at0 parallel outer ~n:n_outer with
          | Some outer ->
            let #(outer0, outer1) = outer in
            Pair_or_null.some (Prod (outer0, inner)) (Prod (outer1, inner))
          | None ->
            (*  ((n_outer = 0) or (n_outer = len_outer)) and (n % len_inner = 0)
             -> (n = 0) or (n = len)
             -> unreachable *)
            assert false)
        else (
          let n_outer = n / len_inner in
          let n_inner = n % len_inner in
          match%optional_u.Pair_or_null
            split_at0 parallel outer ~n:n_outer, split_at1 parallel inner ~n:n_inner
          with
          | Some outer', Some inner' ->
            let #(outer0, outer1) = outer' in
            let #(inner0, inner1) = inner' in
            (match%optional_u.Pair_or_null next0 parallel outer1 with
             | Some a_outer1 ->
               let #(a, outer1) = a_outer1 in
               let seq0 = Consr (Prod (outer0, inner), a, inner0) in
               let seq1 = Consl (a, inner1, Prod (outer1, inner)) in
               Pair_or_null.some seq0 seq1
             | None ->
               (*  length outer1 = 0
                -> unreachable *)
               assert false)
          | None, Some inner' ->
            (*  ((n_outer = 0) or (n_outer = len_outer)) and (n < len)
             -> n < len_inner
             -> splitting seq1 preserves order *)
            let #(inner0, inner1) = inner' in
            (match%optional_u.Pair_or_null next0 parallel outer with
             | Some a_outer ->
               let #(a, outer) = a_outer in
               let seq0 = One (a, inner0) in
               let seq1 = Consl (a, inner1, Prod (outer, inner)) in
               Pair_or_null.some seq0 seq1
             | None ->
               (*  length outer1 = 0
                -> unreachable *)
               assert false)
          | _, None ->
            (*  (n_inner = 0) or (n_inner = len_inner)
             -> n % len_inner = 0
             -> unreachable *)
            assert false)
    ;;

    let split_at_product ~length0 ~length1 ~next0 ~next1 ~split_at0 ~split_at1 =
      ();
      fun parallel t ~n ->
        split_at_prod ~length0 ~length1 ~next0 ~next1 ~split_at0 ~split_at1 parallel t ~n
    ;;
  end

  let product_left (Known seq0 : _ t) (Known seq1 : _ t) = exclave_
    Known
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:seq0.next ~next1:seq1.next
      ; split_at =
          Product.split_at_product
            ~length0:seq0.length
            ~length1:seq1.length
            ~next0:seq0.next
            ~next1:seq1.next
            ~split_at0:seq0.split_at
            ~split_at1:seq1.split_at
      ; length = Product.length_product ~length0:seq0.length ~length1:seq1.length
      }
  ;;

  let product_right t0 t1 = exclave_
    let seq = product_left t1 t0 in
    map seq ~f:(fun (b, a) : ('a * 'b) -> a, b) [@nontail]
  ;;

  let iteri' parallel t ~f =
    match indexed t with
    | Known ({ current; next; _ } as known) ->
      parallel_fold
        parallel
        ~f:(fun parallel () (i, a) -> f parallel i a)
        ~init:(fun () -> ())
        ~state:current
        ~next
        ~split:(split_middle known)
        ~combine:(fun _ () () -> ()) [@nontail]
  ;;

  let iteri parallel t ~f = iteri' parallel t ~f:(fun _ a -> f a) [@nontail]

  let fold' parallel (t : _ t) ~init ~f ~combine =
    match t with
    | Known ({ current; next; _ } as known) ->
      parallel_fold
        parallel
        ~f
        ~init
        ~state:current
        ~next
        ~split:(split_middle known)
        ~combine [@nontail]
  ;;

  let fold parallel t ~init ~f ~combine =
    fold'
      parallel
      t
      ~init
      ~f:(fun _ acc a -> f acc a)
      ~combine:(fun _ a b -> combine a b) [@nontail]
  ;;

  let foldi' parallel t ~init ~f ~combine =
    fold'
      parallel
      (indexed t)
      ~init
      ~f:(fun parallel acc (i, a) -> f parallel i acc a)
      ~combine [@nontail]
  ;;

  let foldi parallel t ~init ~f ~combine =
    foldi'
      parallel
      t
      ~init
      ~f:(fun _ i acc a -> f i acc a)
      ~combine:(fun _ a b -> combine a b) [@nontail]
  ;;

  let iter' parallel t ~f =
    fold'
      parallel
      t
      ~init:(fun () -> ())
      ~f:(fun parallel () a -> f parallel a)
      ~combine:(fun _ () () -> ())
  ;;

  let iter parallel t ~f = iter' parallel t ~f:(fun _ a -> f a) [@nontail]

  let reduce' parallel (t : 'a t) ~f =
    fold'
      parallel
      t
      ~init:(fun () : 'a option -> None)
      ~f:(fun parallel acc a ->
        match acc with
        | Some acc -> Some (f parallel acc a)
        | None -> Some a)
      ~combine:(fun parallel a b ->
        Option.merge ~f:(fun a b -> f parallel a b) a b [@nontail])
  ;;

  let reduce parallel t ~f = reduce' parallel t ~f:(fun _ a -> f a) [@nontail]

  let find' parallel (t : 'a t) ~f =
    fold'
      parallel
      t
      ~init:(fun () : 'a option -> None)
      ~f:(fun parallel acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel a then Some a else None)
      ~combine:(fun _ a b -> Option.first_some a b)
  ;;

  let find parallel t ~f = find' parallel t ~f:(fun _ a -> f a) [@nontail]

  let findi' parallel (t : 'a t) ~f =
    foldi'
      parallel
      t
      ~init:(fun () : (int * 'a) option -> None)
      ~f:(fun parallel i acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel i a then Some (i, a) else None)
      ~combine:(fun _ a b -> Option.first_some a b) [@nontail]
  ;;

  let findi parallel t ~f = findi' parallel t ~f:(fun _ i a -> f i a) [@nontail]

  let to_list parallel (t : 'a t) =
    match t with
    | Known ({ current; next; _ } as known) ->
      parallel_fold
        parallel
        ~init:(fun () : 'a list -> [])
        ~f:(fun _ acc a : 'a list -> a :: acc)
        ~state:current
        ~next
        ~split:(split_middle known)
        ~combine:(fun _ a b -> b @ a)
      |> List.rev
  ;;

  let unsafe_to_array parallel (Known seq : _ t) =
    let length = seq.length seq.current in
    match%optional_u.Pair_or_null seq.next parallel seq.current with
    | None -> [||]
    | Some a_current ->
      let #(a, current) = a_current in
      let arr = Array.create ~len:length a in
      iteri'
        parallel
        (Known { seq with current })
        ~f:(fun _ i a -> Array.unsafe_racy_set_contended arr (i + 1) a);
      arr
  ;;

  let to_iarray parallel t =
    unsafe_to_array parallel t |> Iarray.unsafe_of_array__promise_no_mutation
  ;;
end

type 'a t = ('a, [ `Unknown | `Known ]) seq

let[@inline always] globalize : 'a t @ local -> 'a t = function
  | Unknown { current; next; split } -> Unknown { current; next; split }
  | Known { current; next; split_at; length } -> Known { current; next; split_at; length }
;;

let[@inline always] of_with_length : _ With_length.t @ local -> _ t @ local =
  fun (Known _ as t) -> t
;;

let unfold ~init ~next ~split = exclave_ Unknown { current = init; next; split }
let empty = With_length.empty
let range = With_length.range
let init' = With_length.init'
let init = With_length.init
let of_iarray = With_length.of_iarray

let map' t ~f = exclave_
  match t with
  | Unknown ({ next; _ } as unknown) ->
    let next parallel current =
      match%optional_u.Pair_or_null next parallel current with
      | Some a_current ->
        let #(a, current) = a_current in
        Pair_or_null.some (f parallel a) current
      | None -> Pair_or_null.none ()
    in
    Unknown { unknown with next }
  | Known _ as t -> With_length.map' t ~f
;;

let map t ~f = exclave_ map' t ~f:(fun _ a -> f a) [@nontail]

let filter_map' t ~f = exclave_
  match t with
  | Unknown ({ next; _ } as unknown) ->
    let[@loop] rec next' parallel current =
      match%optional_u.Pair_or_null next parallel current with
      | None -> Pair_or_null.none ()
      | Some a_current ->
        let #(a, current) = a_current in
        (match f parallel a with
         | None -> next' parallel current
         | Some a -> Pair_or_null.some a current)
    in
    Unknown { unknown with next = next' }
  | Known _ as t -> With_length.filter_map' t ~f
;;

let filter_map t ~f = exclave_ filter_map' t ~f:(fun _ a -> f a) [@nontail]

module Append = struct
  include With_length.Append

  let split_append ~split0 ~split1 =
    ();
    fun parallel t ->
      match t with
      | Left s0 ->
        (match%optional_u.Pair_or_null split0 parallel s0 with
         | Some s0 ->
           let #(s00, s01) = s0 in
           Pair_or_null.some (Left s00) (Left s01)
         | None -> Pair_or_null.none ())
      | Right s1 ->
        (match%optional_u.Pair_or_null split1 parallel s1 with
         | Some s1 ->
           let #(s10, s11) = s1 in
           Pair_or_null.some (Right s10) (Right s11)
         | None -> Pair_or_null.none ())
      | Both (s0, s1) -> Pair_or_null.some (Left s0) (Right s1)
  ;;
end

let append seq0 seq1 = exclave_
  match seq0, seq1 with
  | (Known _ as seq0), (Known _ as seq1) -> With_length.append seq0 seq1
  | Known seq0, Unknown seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:seq1.next
      ; split =
          Append.split_append ~split0:(With_length.split_middle seq0) ~split1:seq1.split
      }
  | Unknown seq0, Known seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:seq1.next
      ; split =
          Append.split_append ~split0:seq0.split ~split1:(With_length.split_middle seq1)
      }
  | Unknown seq0, Unknown seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:seq1.next
      ; split = Append.split_append ~split0:seq0.split ~split1:seq1.split
      }
;;

module Concat = struct
  type%fuelproof (_
                 , 'ss : value mod contended portable unyielding)
                 t :
                 value mod contended portable unyielding =
    | All of 'ss
    | One : (_, 'a) unknown -> ('a, 'ss) t
    | Cons : (_, 'a) unknown * 'ss -> ('a, 'ss) t

  let create ss = All ss

  let rec next_all
    : type (ss : value mod contended portable unyielding).
      next:(_ @ local -> ss -> (_, ss) Pair_or_null.t) @ portable unyielding
      -> _ @ local
      -> ss
      -> (_, (_, ss) t) Pair_or_null.t
    =
    fun ~next parallel ss ->
    match%optional_u.Pair_or_null next parallel ss with
    | Some seq_ss ->
      (match seq_ss with
       | #(Known seq, ss) ->
         let seq =
           { current = seq.current
           ; next = seq.next
           ; split = With_length.split_middle seq
           }
         in
         next_cons ~next parallel seq ss
       | #(Unknown seq, ss) -> next_cons ~next parallel seq ss)
    | None -> Pair_or_null.none ()

  and next_cons
    : type (s : value mod contended portable unyielding) (ss :
                                                         value
                                                         mod contended portable unyielding).
      next:(_ @ local -> ss -> (_, ss) Pair_or_null.t) @ portable unyielding
      -> _ @ local
      -> (s, _) unknown @ local
      -> ss
      -> (_, (_, ss) t) Pair_or_null.t
    =
    fun ~next parallel seq ss ->
    match%optional_u.Pair_or_null seq.next parallel seq.current with
    | Some a_current ->
      let #(a, current) = a_current in
      Pair_or_null.some a (Cons ({ seq with current }, ss))
    | None -> next_all ~next parallel ss
  ;;

  let next_concat ~next =
    ();
    fun parallel t ->
      match t with
      | All all -> next_all ~next parallel all
      | Cons (seq, ss) -> next_cons ~next parallel seq ss
      | One seq ->
        (match%optional_u.Pair_or_null seq.next parallel seq.current with
         | Some a_current ->
           let #(a, current) = a_current in
           Pair_or_null.some a (One { seq with current })
         | None -> Pair_or_null.none ())
  ;;

  let split_concat ~next ~split =
    ();
    fun parallel t ->
      match t with
      | All ss ->
        (match%optional_u.Pair_or_null split parallel ss with
         | Some ss ->
           let #(ss0, ss1) = ss in
           Pair_or_null.some (All ss0) (All ss1)
         | None ->
           (match%optional_u.Pair_or_null next parallel ss with
            | Some seq_ss ->
              (match seq_ss with
               | #(Known seq, ss) ->
                 let seq =
                   { current = seq.current
                   ; next = seq.next
                   ; split = With_length.split_middle seq
                   }
                 in
                 Pair_or_null.some (One seq) (All ss)
               | #(Unknown seq, ss) -> Pair_or_null.some (One seq) (All ss))
            | None -> Pair_or_null.none ()))
      | One seq ->
        (match%optional_u.Pair_or_null seq.split parallel seq.current with
         | Some current ->
           let #(current0, current1) = current in
           Pair_or_null.some
             (One { seq with current = current0 })
             (One { seq with current = current1 })
         | None -> Pair_or_null.none ())
      | Cons (seq, ss) ->
        (match%optional_u.Pair_or_null split parallel ss with
         | Some ss ->
           let #(ss0, ss1) = ss in
           Pair_or_null.some (Cons (seq, ss0)) (All ss1)
         | None -> Pair_or_null.some (One seq) (All ss))
  ;;
end

let concat seqs = exclave_
  match seqs with
  | Known seqs ->
    Unknown
      { current = Concat.create seqs.current
      ; next = Concat.next_concat ~next:seqs.next
      ; split = Concat.split_concat ~next:seqs.next ~split:(With_length.split_middle seqs)
      }
  | Unknown seqs ->
    Unknown
      { current = Concat.create seqs.current
      ; next = Concat.next_concat ~next:seqs.next
      ; split = Concat.split_concat ~next:seqs.next ~split:seqs.split
      }
;;

module Product = struct
  type ('s0 : value mod contended portable unyielding
       , 's1 : value mod contended portable unyielding
       , 'a : value mod contended portable unyielding)
       t =
    | One of #('a * row:'s1)
    | Cons of #('a * row:'s1 * 's0 * 's1)
    | Prod of 's0 * 's1

  let create s0 s1 = Prod (s0, s1)

  let rec next_cons ~next0 ~next1 parallel a row s0 s1 =
    match%optional_u.Pair_or_null next1 parallel row with
    | Some b_row ->
      let #(b, row) = b_row in
      Pair_or_null.some (a, b) (Cons #(a, ~row, s0, s1))
    | None -> next_prod ~next0 ~next1 parallel s0 s1

  and next_prod ~next0 ~next1 parallel s0 s1 =
    match%optional_u.Pair_or_null next0 parallel s0 with
    | Some a_s0 ->
      let #(a, s0) = a_s0 in
      next_cons ~next0 ~next1 parallel a s1 s0 s1
    | None -> Pair_or_null.none ()
  ;;

  let next_product ~next0 ~next1 =
    ();
    fun parallel t ->
      match t with
      | Prod (s0, s1) -> next_prod ~next0 ~next1 parallel s0 s1
      | Cons #(a, ~row, s0, s1) -> next_cons ~next0 ~next1 parallel a row s0 s1
      | One #(a, ~row) ->
        (match%optional_u.Pair_or_null next1 parallel row with
         | Some b_row ->
           let #(b, row) = b_row in
           Pair_or_null.some (a, b) (One #(a, ~row))
         | None -> Pair_or_null.none ())
  ;;

  let split_prod ~split0 ~split1 parallel s0 s1 =
    match%optional_u.Pair_or_null split0 parallel s0 with
    | Some s0 ->
      let #(s00, s01) = s0 in
      Pair_or_null.some (s00, s1) (s01, s1)
    | None ->
      (* length seq0 <= 1 -> splitting seq1 preserves order *)
      (match%optional_u.Pair_or_null split1 parallel s1 with
       | Some s1 ->
         let #(s10, s11) = s1 in
         Pair_or_null.some (s0, s10) (s0, s11)
       | None -> Pair_or_null.none ())
  ;;

  let split_cons ~split0 ~split1 parallel a row s0 s1 =
    match%optional_u.Pair_or_null split_prod ~split0 ~split1 parallel s0 s1 with
    | Some s0_s1 ->
      let #((s00, s10), (s01, s11)) = s0_s1 in
      Pair_or_null.some (Cons #(a, ~row, s00, s10)) (Prod (s01, s11))
    | None -> Pair_or_null.some (One #(a, ~row)) (Prod (s0, s1))
  ;;

  let split_product ~split0 ~split1 =
    ();
    fun parallel t ->
      match t with
      | Cons #(a, ~row, s0, s1) -> split_cons ~split0 ~split1 parallel a row s0 s1
      | Prod (s0, s1) ->
        (match%optional_u.Pair_or_null split_prod ~split0 ~split1 parallel s0 s1 with
         | Some s0_s1 ->
           let #((s00, s10), (s01, s11)) = s0_s1 in
           Pair_or_null.some (Prod (s00, s10)) (Prod (s01, s11))
         | None -> Pair_or_null.none ())
      | One #(a, ~row) ->
        (match%optional_u.Pair_or_null split1 parallel row with
         | Some row ->
           let #(row0, row1) = row in
           Pair_or_null.some (One #(a, ~row:row0)) (One #(a, ~row:row1))
         | None -> Pair_or_null.none ())
  ;;
end

let product_left seq0 seq1 = exclave_
  match seq0, seq1 with
  | (Known _ as seq0), (Known _ as seq1) -> With_length.product_left seq0 seq1
  | Known seq0, Unknown seq1 ->
    Unknown
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:seq0.next ~next1:seq1.next
      ; split =
          Product.split_product ~split0:(With_length.split_middle seq0) ~split1:seq1.split
      }
  | Unknown seq0, Known seq1 ->
    Unknown
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:seq0.next ~next1:seq1.next
      ; split =
          Product.split_product ~split0:seq0.split ~split1:(With_length.split_middle seq1)
      }
  | Unknown seq0, Unknown seq1 ->
    Unknown
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:seq0.next ~next1:seq1.next
      ; split = Product.split_product ~split0:seq0.split ~split1:seq1.split
      }
;;

let product_right t0 t1 = exclave_
  let seq = product_left t1 t0 in
  map seq ~f:(fun (b, a) : ('a * 'b) -> a, b) [@nontail]
;;

let concat_map' t ~f = exclave_ concat (map' t ~f)
let concat_map t ~f = exclave_ concat (map t ~f)

let fold' parallel t ~init ~f ~combine =
  match t with
  | Unknown { current; next; split } ->
    parallel_fold parallel ~init ~f ~state:current ~next ~split ~combine [@nontail]
  | Known _ as t -> With_length.fold' parallel t ~init ~f ~combine
;;

let fold parallel t ~init ~f ~combine =
  fold'
    parallel
    t
    ~init
    ~f:(fun _ acc a -> f acc a)
    ~combine:(fun _ a b -> combine a b) [@nontail]
;;

let iter' parallel t ~f =
  fold'
    parallel
    t
    ~init:(fun () -> ())
    ~f:(fun parallel () a -> f parallel a)
    ~combine:(fun _ () () -> ()) [@nontail]
;;

let iter parallel t ~f = iter' parallel t ~f:(fun _ a -> f a) [@nontail]

let reduce' parallel (t : 'a t) ~f =
  fold'
    parallel
    t
    ~init:(fun () : 'a option -> None)
    ~f:(fun parallel acc a ->
      match acc with
      | Some acc -> Some (f parallel acc a)
      | None -> Some a)
    ~combine:(fun parallel a b ->
      Option.merge ~f:(fun a b -> f parallel a b) a b [@nontail])
;;

let reduce parallel t ~f = reduce' parallel t ~f:(fun _ acc a -> f acc a) [@nontail]

let find' parallel (t : 'a t) ~f =
  fold'
    parallel
    t
    ~init:(fun () : 'a option -> None)
    ~f:(fun parallel acc a ->
      match acc with
      | Some _ -> acc
      | None -> if f parallel a then Some a else None)
    ~combine:(fun _ a b -> Option.first_some a b) [@nontail]
;;

let find parallel t ~f = find' parallel t ~f:(fun _ a -> f a) [@nontail]

let to_list parallel (t : 'a t) =
  match t with
  | Unknown { current; next; split } ->
    parallel_fold
      parallel
      ~init:(fun () : 'a list -> [])
      ~f:(fun _ acc a : 'a list -> a :: acc)
      ~state:current
      ~next
      ~split
      ~combine:(fun _ a b -> b @ a)
    |> List.rev
  | Known _ as t -> With_length.to_list parallel t
;;

let to_iarray parallel t =
  match t with
  | Unknown _ -> to_list parallel t |> Iarray.of_list
  | Known _ as t -> With_length.to_iarray parallel t
;;
