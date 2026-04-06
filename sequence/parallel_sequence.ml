open! Base
open! Import
include Parallel_sequence_intf

module%template Option_u = struct
  [@@@kind k = (value_or_null & value_or_null)]

  type 'a t = ('a Option_u.t[@kind k])

  let none = (Option_u.none [@kind k])
  let some = (Option_u.some [@kind k])
end

type ('s : value_or_null mod contended portable, 'a : value_or_null) unknown =
  { current : 's @@ global
  ; next : Parallel_kernel.t @ local -> 's -> #('a * 's) Option_u.t @@ global portable
  ; split : Parallel_kernel.t @ local -> 's -> #('s * 's) Option_u.t @@ global portable
  }

type ('s : value_or_null mod contended portable, 'a : value_or_null) known =
  { current : 's @@ global
  ; next : Parallel_kernel.t @ local -> 's -> #('a * 's) @@ global portable
  ; split_at : Parallel_kernel.t @ local -> 's -> n:int -> #('s * 's) @@ global portable
  ; length : 's -> int @@ global portable
  }

type (_ : value_or_null, _) seq =
  | Unknown :
      ('a : value_or_null) ('s : value_or_null mod contended portable).
      ('s, 'a) unknown
      -> ('a, [> `Unknown ]) seq
  | Known :
      ('a : value_or_null) ('s : value_or_null mod contended portable).
      ('s, 'a) known
      -> ('a, [> `Known ]) seq

let parallel_fold
  (type (a : value_or_null) (state : value_or_null mod shareable shared))
  parallel
  ~f
  ~init
  ~state
  ~next
  ~split
  ~combine
  =
  Parallel_kernel.fold
    parallel
    ~init
    ~state
    ~next:(fun parallel acc state ->
      match (next parallel state : #(a * state) Option_u.t) with
      | T #(Some, #(a, state)) -> Option_u.some #(f parallel acc a, state)
      | T #(None, _) -> Option_u.none ())
    ~stop:(fun _ acc -> acc)
    ~fork:split
    ~join:combine [@nontail]
;;

module With_length = struct
  type ('a : value_or_null) t = ('a, [ `Known ]) seq

  let[@inline] globalize : _ t @ local -> _ t = function
    | Known { current; next; split_at; length } ->
      Known { current; next; split_at; length }
  ;;

  let length : _ t @ local -> int = function
    | Known { current; length; _ } -> length current
  ;;

  let[@inline] split_opt { split_at; length; _ } =
    ();
    fun parallel current ->
      let len = length current in
      if len < 2
      then Option_u.none ()
      else Option_u.some (split_at parallel current ~n:(len / 2))
  ;;

  let[@inline] next_opt { next; length; _ } =
    ();
    fun parallel current ->
      if length current = 0
      then Option_u.none ()
      else Option_u.some (next parallel current)
  ;;

  let unfold ~init ~next ~split_at ~length = exclave_
    Known { current = init; next; split_at; length }
  ;;

  let empty =
    Known
      { current = ()
      ; next = (fun _ () -> assert false)
      ; split_at = (fun _ () ~n:_ -> assert false)
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
        ; next = (fun _ (~start, ~stop) -> #(start, (~start:(start + stride), ~stop)))
        ; split_at =
            (fun _ (~start, ~stop) ~n ->
              let pivot = start + (n * stride) in
              #((~start, ~stop:pivot), (~start:pivot, ~stop)))
        })
  ;;

  let map (t : _ t) ~f = exclave_
    match t with
    | Known ({ next; _ } as known) ->
      let next parallel current =
        let #(a, current) = next parallel current in
        #(f parallel a, current)
      in
      Known { known with next }
  ;;

  let filter_map (t : _ t) ~f = exclave_
    match t with
    | Known ({ length; current; next; _ } as known) ->
      let[@loop] rec next' parallel current =
        if length current = 0
        then Option_u.none ()
        else (
          let #(a, current) = next parallel current in
          match f parallel a with
          | None -> next' parallel current
          | Some a -> Option_u.some #(a, current))
      in
      Unknown { current; next = next'; split = split_opt known }
  ;;

  let init n ~f = exclave_
    let ints = range 0 n in
    map ints ~f
  ;;

  let of_iarray
    (type a : value_or_null mod contended portable separable)
    (iarray : a iarray)
    = exclave_
    Known
      { current =
          ((iarray, ~start:0, ~stop:(Iarray.length iarray))
           : a iarray * start:int * stop:int)
      ; length = (fun (_, ~start, ~stop) -> stop - start)
      ; next =
          (fun _ (iarray, ~start, ~stop) ->
            #(Iarray.unsafe_get iarray start, (iarray, ~start:(start + 1), ~stop)))
      ; split_at =
          (fun _ (iarray, ~start, ~stop) ~n ->
            let pivot = start + n in
            #((iarray, ~start, ~stop:pivot), (iarray, ~start:pivot, ~stop)))
      }
  ;;

  let zip_exn (t0 : _ t) (t1 : _ t) : _ t = exclave_
    match t0, t1 with
    | ( Known (type (s0 : value_or_null mod contended portable))
          ({ current = current0; next = next0; split_at = split_at0; length = length0 } :
            (s0, _) known)
      , Known (type (s1 : value_or_null mod contended portable))
          ({ current = current1; next = next1; split_at = split_at1; length = length1 } :
            (s1, _) known) ) ->
      if length0 current0 <> length1 current1
      then invalid_arg "Parallel_sequence.With_length.zip_exn";
      Known
        { current = ((current0, current1) : s0 * s1)
        ; next =
            (fun parallel (current0, current1) ->
              let #(a0, current0) = next0 parallel current0 in
              let #(a1, current1) = next1 parallel current1 in
              #((a0, a1), (current0, current1)))
        ; split_at =
            (fun parallel (current0, current1) ~n ->
              let #(current00, current01) = split_at0 parallel current0 ~n in
              let #(current10, current11) = split_at1 parallel current1 ~n in
              #((current00, current10), (current01, current11)))
        ; length = (fun (current0, _) -> length0 current0)
        }
  ;;

  let indexed t = exclave_ zip_exn (range 0 (length t)) t

  let mapi t ~f = exclave_
    map (indexed t) ~f:(fun parallel (i, a) -> f parallel i a) [@nontail]
  ;;

  module Append = struct
    type ('s0 : value_or_null mod contended portable
         , 's1 : value_or_null mod contended portable)
         t =
      | Left of 's0
      | Right of 's1
      | Both of 's0 * 's1

    let create s0 s1 = Both (s0, s1)

    let next_left ~next0 parallel s0 =
      let #(a, current) = next0 parallel s0 in
      #(a, Left current)
    ;;

    let next_right ~next1 parallel s1 =
      let #(a, current) = next1 parallel s1 in
      #(a, Right current)
    ;;

    let next_append ~next0 ~length0 ~next1 =
      ();
      fun parallel t ->
        match t with
        | Left s0 -> next_left ~next0 parallel s0
        | Right s1 -> next_right ~next1 parallel s1
        | Both (s0, s1) ->
          if length0 s0 = 0
          then next_right ~next1 parallel s1
          else (
            let #(a, s0) = next0 parallel s0 in
            #(a, Both (s0, s1)))
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
          then #(Left s0, Right s1)
          else if n < len0
          then (
            let #(s00, s01) = split_at0 parallel s0 ~n in
            #(Left s00, Both (s01, s1)))
          else (
            let #(s10, s11) = split_at1 parallel s1 ~n:(n - len0) in
            #(Both (s0, s10), Right s11))
        | Left s0 ->
          let #(s00, s01) = split_at0 parallel s0 ~n in
          #(Left s00, Left s01)
        | Right s1 ->
          let #(s10, s11) = split_at1 parallel s1 ~n in
          #(Right s10, Right s11)
    ;;
  end

  let append (Known seq0 : _ t) (Known seq1 : _ t) = exclave_
    Known
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~length0:seq0.length ~next1:seq1.next
      ; split_at =
          Append.split_at_append
            ~length0:seq0.length
            ~split_at0:seq0.split_at
            ~split_at1:seq1.split_at
      ; length = Append.length_append ~length0:seq0.length ~length1:seq1.length
      }
  ;;

  module Product = struct
    type ('s0 : value_or_null mod contended portable
         , 's1 : value_or_null mod contended portable
         , 'a : value_or_null mod contended portable)
         t :
         value mod contended portable =
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

    let rec next_prod
      : type (a : value_or_null mod contended portable) b (s0 :
                                                          value_or_null
                                                          mod contended portable) (s1 :
                                                                                  value_or_null
                                                                                  mod
                                                                                     contended
                                                                                     portable).
        next0:(_ @ local -> s0 -> #(a * s0))
        -> length0:(s0 -> int)
        -> next1:(_ @ local -> s1 -> #(b * s1))
        -> length1:(s1 -> int)
        -> _ @ local
        -> (s0, s1, a) t
        -> #((a * b) * (s0, s1, a) t)
      =
      fun ~next0 ~length0 ~next1 ~length1 parallel t ->
      match t with
      | One (a, s1) ->
        let #(b, s1) = next1 parallel s1 in
        #((a, b), One (a, s1))
      | Prod (s0, s1) ->
        let #(a, s0) = next0 parallel s0 in
        next_consl ~next0 ~length0 ~next1 ~length1 parallel a s1 (Prod (s0, s1))
      | Consl (a, s1, t) -> next_consl ~next0 ~length0 ~next1 ~length1 parallel a s1 t
      | Consr (t, a, s1) -> next_consr ~next0 ~length0 ~next1 ~length1 parallel t a s1

    and next_consl
      : type (a : value_or_null mod contended portable) b (s0 :
                                                          value_or_null
                                                          mod contended portable) (s1 :
                                                                                  value_or_null
                                                                                  mod
                                                                                     contended
                                                                                     portable).
        next0:(_ @ local -> s0 -> #(a * s0))
        -> length0:(s0 -> int)
        -> next1:(_ @ local -> s1 -> #(b * s1))
        -> length1:(s1 -> int)
        -> _ @ local
        -> a
        -> s1
        -> (s0, s1, a) t
        -> #((a * b) * (s0, s1, a) t)
      =
      fun ~next0 ~length0 ~next1 ~length1 parallel a s1 t ->
      if length1 s1 = 0
      then next_prod ~next0 ~length0 ~next1 ~length1 parallel t
      else (
        let #(b, s1) = next1 parallel s1 in
        #((a, b), Consl (a, s1, t)))

    and next_consr
      : type (a : value_or_null mod contended portable) b (s0 :
                                                          value_or_null
                                                          mod contended portable) (s1 :
                                                                                  value_or_null
                                                                                  mod
                                                                                     contended
                                                                                     portable).
        next0:(_ @ local -> s0 -> #(a * s0))
        -> length0:(s0 -> int)
        -> next1:(_ @ local -> s1 -> #(b * s1))
        -> length1:(s1 -> int)
        -> _ @ local
        -> (s0, s1, a) t
        -> a
        -> s1
        -> #((a * b) * (s0, s1, a) t)
      =
      fun ~next0 ~length0 ~next1 ~length1 parallel t a s1 ->
      if length_prod ~length0 ~length1 t = 0
      then (
        let #(b, s1) = next1 parallel s1 in
        #((a, b), Consr (t, a, s1)))
      else (
        let #(ab, t) = next_prod ~next0 ~length0 ~next1 ~length1 parallel t in
        #(ab, Consr (t, a, s1)))
    ;;

    let next_product ~next0 ~length0 ~next1 ~length1 =
      ();
      fun parallel t -> next_prod ~next0 ~length0 ~next1 ~length1 parallel t
    ;;

    let rec split_at_prod
      : type (a : value_or_null mod contended portable) b (s0 :
                                                          value_or_null
                                                          mod contended portable) (s1 :
                                                                                  value_or_null
                                                                                  mod
                                                                                     contended
                                                                                     portable).
        next0:(_ @ local -> s0 -> #(a * s0))
        -> next1:(_ @ local -> s1 -> #(b * s1))
        -> length0:(s0 -> int) @ portable
        -> length1:(s1 -> int) @ portable
        -> split_at0:(_ @ local -> s0 -> n:int -> #(s0 * s0))
        -> split_at1:(_ @ local -> s1 -> n:int -> #(s1 * s1))
        -> _ @ local
        -> (s0, s1, a) t
        -> n:int
        -> #((s0, s1, a) t * (s0, s1, a) t)
      =
      fun ~next0 ~next1 ~length0 ~length1 ~split_at0 ~split_at1 parallel t ~n ->
      match t with
      | One (a, s1) ->
        let #(s10, s11) = split_at1 parallel s1 ~n in
        #(One (a, s10), One (a, s11))
      | Consl (a, s1, t) ->
        let len = length1 s1 in
        (match Ordering.of_int (compare n len) with
         | Equal -> #(One (a, s1), t)
         | Less ->
           let #(s10, s11) = split_at1 parallel s1 ~n in
           #(One (a, s10), Consl (a, s11, t))
         | Greater ->
           let #(t0, t1) =
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
           in
           #(Consl (a, s1, t0), t1))
      | Consr (t, a, s1) ->
        let len = length_product ~length0 ~length1 t in
        (match Ordering.of_int (compare n len) with
         | Equal -> #(t, One (a, s1))
         | Less ->
           let #(t0, t1) =
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
           in
           #(t0, Consr (t1, a, s1))
         | Greater ->
           let #(s10, s11) = split_at1 parallel s1 ~n:(n - len) in
           #(Consr (t, a, s10), One (a, s11)))
      | Prod (outer, inner) ->
        let len_outer = length0 outer in
        let len_inner = length1 inner in
        if n % len_inner = 0
        then (
          let n_outer = n / len_inner in
          let #(outer0, outer1) = split_at0 parallel outer ~n:n_outer in
          #(Prod (outer0, inner), Prod (outer1, inner)))
        else (
          let n_outer = n / len_inner in
          let n_inner = n % len_inner in
          if n_outer = 0 || len_outer < 2
          then (
            let #(inner0, inner1) = split_at1 parallel inner ~n:n_inner in
            let #(a, outer) = next0 parallel outer in
            let seq0 = One (a, inner0) in
            let seq1 = Consl (a, inner1, Prod (outer, inner)) in
            #(seq0, seq1))
          else (
            let #(outer0, outer1) = split_at0 parallel outer ~n:n_outer in
            let #(inner0, inner1) = split_at1 parallel inner ~n:n_inner in
            let #(a, outer1) = next0 parallel outer1 in
            let seq0 = Consr (Prod (outer0, inner), a, inner0) in
            let seq1 = Consl (a, inner1, Prod (outer1, inner)) in
            #(seq0, seq1)))
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
      ; next =
          Product.next_product
            ~next0:seq0.next
            ~length0:seq0.length
            ~next1:seq1.next
            ~length1:seq1.length
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
    map seq ~f:(fun _ (b, a) : ('a * 'b) -> a, b) [@nontail]
  ;;

  let iteri parallel t ~f =
    match indexed t with
    | Known ({ current; _ } as known) ->
      parallel_fold
        parallel
        ~f:(fun parallel () (i, a) -> f parallel i a)
        ~init:(fun () -> ())
        ~state:current
        ~next:(next_opt known)
        ~split:(split_opt known)
        ~combine:(fun _ () () -> ()) [@nontail]
  ;;

  let fold parallel (t : _ t) ~init ~f ~combine =
    match t with
    | Known ({ current; _ } as known) ->
      parallel_fold
        parallel
        ~f
        ~init
        ~state:current
        ~next:(next_opt known)
        ~split:(split_opt known)
        ~combine [@nontail]
  ;;

  let foldi parallel t ~init ~f ~combine =
    fold
      parallel
      (indexed t)
      ~init
      ~f:(fun parallel acc (i, a) -> f parallel i acc a)
      ~combine [@nontail]
  ;;

  let iter parallel t ~f =
    fold
      parallel
      t
      ~init:(fun () -> ())
      ~f:(fun parallel () a -> f parallel a)
      ~combine:(fun _ () () -> ())
  ;;

  let reduce parallel t ~f =
    fold
      parallel
      t
      ~init:(fun () -> None)
      ~f:(fun parallel acc a ->
        match acc with
        | Some acc -> Some (f parallel acc a)
        | None -> Some a)
      ~combine:(fun parallel a b ->
        Option.merge ~f:(fun a b -> f parallel a b) a b [@nontail])
  ;;

  let find parallel t ~f =
    fold
      parallel
      t
      ~init:(fun () -> None)
      ~f:(fun parallel acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel a then Some a else None)
      ~combine:(fun _ a b -> Option.first_some a b)
  ;;

  let findi parallel t ~f =
    foldi
      parallel
      t
      ~init:(fun () -> None)
      ~f:(fun parallel i acc a ->
        match acc with
        | Some _ -> acc
        | None -> if f parallel i a then Some (i, a) else None)
      ~combine:(fun _ a b -> Option.first_some a b) [@nontail]
  ;;

  let to_list_rev parallel (t : _ t) =
    match t with
    | Known ({ current; _ } as known) ->
      parallel_fold
        parallel
        ~init:(fun () -> [])
        ~f:(fun _ acc a -> a :: acc)
        ~state:current
        ~next:(next_opt known)
        ~split:(split_opt known)
        ~combine:(fun _ a b -> b @ a)
  ;;

  let to_list parallel t = to_list_rev parallel t |> List.rev

  let unsafe_to_array (type a : value_or_null mod non_float) parallel (Known seq : a t)
    : a array
    =
    let length = seq.length seq.current in
    (* Safe because a is non_float and every element is initialized. *)
    let arr = Obj.magic_portable (Array.create ~len:length (Obj.magic None : a)) in
    iteri parallel (Known seq) ~f:(fun _ i a -> Array.unsafe_racy_set_shared arr i a);
    arr
  ;;

  let to_iarray parallel t =
    unsafe_to_array parallel t |> Iarray.unsafe_of_array__promise_no_mutation
  ;;
end

type ('a : value_or_null) t = ('a, [ `Unknown | `Known ]) seq

let[@inline] globalize : ('a : value_or_null). 'a t @ local -> 'a t = function
  | Unknown { current; next; split } -> Unknown { current; next; split }
  | Known { current; next; split_at; length } -> Known { current; next; split_at; length }
;;

let[@inline] of_with_length : _ With_length.t @ local -> _ t @ local =
  fun (Known _ as t) -> t
;;

let unfold ~init ~next ~split = exclave_ Unknown { current = init; next; split }
let empty = With_length.empty
let range = With_length.range
let init = With_length.init
let of_iarray = With_length.of_iarray

let map (type a : value_or_null) t ~f = exclave_
  match t with
  | Unknown (type (s : value_or_null mod contended portable))
      ({ next; _ } as unknown : (s, a) unknown) ->
    let next parallel current =
      match (next parallel current : #(a * s) Option_u.t) with
      | T #(Some, #(a, current)) -> Option_u.some #(f parallel a, current)
      | T #(None, _) -> Option_u.none ()
    in
    Unknown { unknown with next }
  | Known _ as t -> With_length.map t ~f
;;

let filter_map (type a : value_or_null) t ~f = exclave_
  match t with
  | Unknown (type (s : value_or_null mod contended portable))
      ({ next; _ } as unknown : (s, a) unknown) ->
    let[@loop] rec next' parallel current =
      match (next parallel current : #(a * s) Option_u.t) with
      | T #(None, _) -> Option_u.none ()
      | T #(Some, #(a, current)) ->
        (match f parallel a with
         | None -> next' parallel current
         | Some a -> Option_u.some #(a, current))
    in
    Unknown { unknown with next = next' }
  | Known _ as t -> With_length.filter_map t ~f
;;

module Append = struct
  include With_length.Append

  let next_left
    (type (a : value_or_null) (s : value_or_null mod contended portable))
    ~next0
    parallel
    s0
    =
    match (next0 parallel s0 : #(a * s) Option_u.t) with
    | T #(Some, #(a, current)) -> Option_u.some #(a, Left current)
    | T #(None, _) -> Option_u.none ()
  ;;

  let next_right
    (type (a : value_or_null) (s : value_or_null mod contended portable))
    ~next1
    parallel
    s1
    =
    match (next1 parallel s1 : #(a * s) Option_u.t) with
    | T #(Some, #(a, current)) -> Option_u.some #(a, Right current)
    | T #(None, _) -> Option_u.none ()
  ;;

  let next_append
    (type (a : value_or_null) (s : value_or_null mod contended portable))
    ~next0
    ~next1
    =
    ();
    fun parallel t ->
      match t with
      | Left s0 -> next_left ~next0 parallel s0
      | Right s1 -> next_right ~next1 parallel s1
      | Both (s0, s1) ->
        (match (next0 parallel s0 : #(a * s) Option_u.t) with
         | T #(Some, #(a, s0)) -> Option_u.some #(a, Both (s0, s1))
         | T #(None, _) -> next_right ~next1 parallel s1)
  ;;

  let split_append
    (type (s0 : value_or_null mod contended portable)
    (s1 : value_or_null mod contended portable))
    ~split0
    ~split1
    =
    ();
    fun parallel t ->
      match t with
      | Left s0 ->
        (match (split0 parallel s0 : #(s0 * s0) Option_u.t) with
         | T #(Some, #(s00, s01)) -> Option_u.some #(Left s00, Left s01)
         | T #(None, _) -> Option_u.none ())
      | Right s1 ->
        (match (split1 parallel s1 : #(s1 * s1) Option_u.t) with
         | T #(Some, #(s10, s11)) -> Option_u.some #(Right s10, Right s11)
         | T #(None, _) -> Option_u.none ())
      | Both (s0, s1) -> Option_u.some #(Left s0, Right s1)
  ;;
end

let append seq0 seq1 = exclave_
  match seq0, seq1 with
  | (Known _ as seq0), (Known _ as seq1) -> With_length.append seq0 seq1
  | Known seq0, Unknown seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:(With_length.next_opt seq0) ~next1:seq1.next
      ; split =
          Append.split_append ~split0:(With_length.split_opt seq0) ~split1:seq1.split
      }
  | Unknown seq0, Known seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:(With_length.next_opt seq1)
      ; split =
          Append.split_append ~split0:seq0.split ~split1:(With_length.split_opt seq1)
      }
  | Unknown seq0, Unknown seq1 ->
    Unknown
      { current = Append.create seq0.current seq1.current
      ; next = Append.next_append ~next0:seq0.next ~next1:seq1.next
      ; split = Append.split_append ~split0:seq0.split ~split1:seq1.split
      }
;;

module Concat = struct
  type (_ : value_or_null, 'ss : value_or_null mod contended portable) t =
    | All of 'ss
    | One :
        ('a : value_or_null)
        ('s : value_or_null mod contended portable)
        ('ss : value_or_null mod contended portable).
        ('s, 'a) unknown
        -> ('a, 'ss) t
    | Cons :
        ('a : value_or_null)
        ('s : value_or_null mod contended portable)
        ('ss : value_or_null mod contended portable).
        ('s, 'a) unknown * 'ss
        -> ('a, 'ss) t

  let create ss = All ss

  let rec next_all
    : type (a : value_or_null) (ss : value_or_null mod contended portable) k.
      next:(_ @ local -> ss -> #((a, k) seq * ss) Option_u.t) @ portable
      -> _ @ local
      -> ss
      -> #(a * (a, ss) t) Option_u.t
    =
    fun ~next parallel ss ->
    match next parallel ss with
    | T #(Some, #(Known seq, ss)) ->
      let seq =
        { current = seq.current
        ; next = With_length.next_opt seq
        ; split = With_length.split_opt seq
        }
      in
      next_cons ~next parallel seq ss
    | T #(Some, #(Unknown seq, ss)) -> next_cons ~next parallel seq ss
    | T #(None, _) -> Option_u.none ()

  and next_cons
    : type (a : value_or_null) (s : value_or_null mod contended portable) (ss :
                                                                          value_or_null
                                                                          mod
                                                                             contended
                                                                             portable) k.
      next:(_ @ local -> ss -> #((a, k) seq * ss) Option_u.t) @ portable
      -> _ @ local
      -> (s, a) unknown @ local
      -> ss
      -> #(a * (a, ss) t) Option_u.t
    =
    fun ~next parallel seq ss ->
    match seq.next parallel seq.current with
    | T #(Some, #(a, current)) -> Option_u.some #(a, Cons ({ seq with current }, ss))
    | T #(None, _) -> next_all ~next parallel ss
  ;;

  let next_concat (type a : value_or_null) ~next =
    ();
    fun parallel t ->
      match t with
      | All all -> next_all ~next parallel all
      | Cons (seq, ss) -> next_cons ~next parallel seq ss
      | One (type (s : value_or_null mod contended portable)) (seq : (s, a) unknown) ->
        (match (seq.next parallel seq.current : #(a * s) Option_u.t) with
         | T #(Some, #(a, current)) -> Option_u.some #(a, One { seq with current })
         | T #(None, _) -> Option_u.none ())
  ;;

  let split_concat
    (type (a : value_or_null) (ss : value_or_null mod contended portable) k)
    ~next
    ~split
    =
    ();
    fun parallel t ->
      match t with
      | All ss ->
        (match (split parallel ss : #(ss * ss) Option_u.t) with
         | T #(Some, #(ss0, ss1)) -> Option_u.some #(All ss0, All ss1)
         | T #(None, _) ->
           (match (next parallel ss : #((a, k) seq * ss) Option_u.t) with
            | T #(Some, #(Known seq, ss)) ->
              let seq =
                { current = seq.current
                ; next = With_length.next_opt seq
                ; split = With_length.split_opt seq
                }
              in
              Option_u.some #(One seq, All ss)
            | T #(Some, #(Unknown seq, ss)) -> Option_u.some #(One seq, All ss)
            | T #(None, _) -> Option_u.none ()))
      | One seq ->
        (match seq.split parallel seq.current with
         | T #(Some, #(current0, current1)) ->
           Option_u.some
             #(One { seq with current = current0 }, One { seq with current = current1 })
         | T #(None, _) -> Option_u.none ())
      | Cons (seq, ss) ->
        (match split parallel ss with
         | T #(Some, #(ss0, ss1)) -> Option_u.some #(Cons (seq, ss0), All ss1)
         | T #(None, _) -> Option_u.some #(One seq, All ss))
  ;;
end

let concat seqs = exclave_
  match seqs with
  | Known seqs ->
    Unknown
      { current = Concat.create seqs.current
      ; next = Concat.next_concat ~next:(With_length.next_opt seqs)
      ; split =
          Concat.split_concat
            ~next:(With_length.next_opt seqs)
            ~split:(With_length.split_opt seqs)
      }
  | Unknown seqs ->
    Unknown
      { current = Concat.create seqs.current
      ; next = Concat.next_concat ~next:seqs.next
      ; split = Concat.split_concat ~next:seqs.next ~split:seqs.split
      }
;;

module Product = struct
  type ('s0 : value_or_null mod contended portable
       , 's1 : value_or_null mod contended portable
       , 'a : value_or_null mod contended portable)
       t =
    | One of #('a * row:'s1)
    | Cons of #('a * row:'s1 * 's0 * 's1)
    | Prod of 's0 * 's1

  let create s0 s1 = Prod (s0, s1)

  let rec next_cons
    : type (a : value_or_null mod contended portable) (b : value_or_null) (s0 :
                                                                          value_or_null
                                                                          mod
                                                                             contended
                                                                             portable) (s1 :
                                                                                       value_or_null
                                                                                       mod
                                                                                          contended
                                                                                          portable).
      next0:(_ @ local -> s0 -> #(a * s0) Option_u.t)
      -> next1:(_ @ local -> s1 -> #(b * s1) Option_u.t)
      -> _ @ local
      -> a
      -> s1
      -> s0
      -> s1
      -> #((a * b) * (s0, s1, a) t) Option_u.t
    =
    fun ~next0 ~next1 parallel a row s0 s1 ->
    match next1 parallel row with
    | T #(Some, #(b, row)) -> Option_u.some #((a, b), Cons #(a, ~row, s0, s1))
    | T #(None, _) -> next_prod ~next0 ~next1 parallel s0 s1

  and next_prod
    : type (a : value_or_null mod contended portable) (b : value_or_null) (s0 :
                                                                          value_or_null
                                                                          mod
                                                                             contended
                                                                             portable) (s1 :
                                                                                       value_or_null
                                                                                       mod
                                                                                          contended
                                                                                          portable).
      next0:(_ @ local -> s0 -> #(a * s0) Option_u.t)
      -> next1:(_ @ local -> s1 -> #(b * s1) Option_u.t)
      -> _ @ local
      -> s0
      -> s1
      -> #((a * b) * (s0, s1, a) t) Option_u.t
    =
    fun ~next0 ~next1 parallel s0 s1 ->
    match next0 parallel s0 with
    | T #(Some, #(a, s0)) -> next_cons ~next0 ~next1 parallel a s1 s0 s1
    | T #(None, _) -> Option_u.none ()
  ;;

  let next_product
    (type (a : value_or_null) (s : value_or_null mod contended portable))
    ~next0
    ~next1
    =
    ();
    fun parallel t ->
      match t with
      | Prod (s0, s1) -> next_prod ~next0 ~next1 parallel s0 s1
      | Cons #(a, ~row, s0, s1) -> next_cons ~next0 ~next1 parallel a row s0 s1
      | One #(a, ~row) ->
        (match (next1 parallel row : #(a * s) Option_u.t) with
         | T #(Some, #(b, row)) -> Option_u.some #((a, b), One #(a, ~row))
         | T #(None, _) -> Option_u.none ())
  ;;

  let split_prod
    (type (s0 : value_or_null mod contended portable)
    (s1 : value_or_null mod contended portable))
    ~split0
    ~split1
    parallel
    s0
    s1
    =
    match (split0 parallel s0 : #(s0 * s0) Option_u.t) with
    | T #(Some, #(s00, s01)) -> Option_u.some #((s00, s1), (s01, s1))
    | T #(None, _) ->
      (* length seq0 <= 1 -> splitting seq1 preserves order *)
      (match (split1 parallel s1 : #(s1 * s1) Option_u.t) with
       | T #(Some, #(s10, s11)) -> Option_u.some #((s0, s10), (s0, s11))
       | T #(None, _) -> Option_u.none ())
  ;;

  let split_cons
    (type (s0 : value_or_null mod contended portable)
    (s1 : value_or_null mod contended portable))
    ~split0
    ~split1
    parallel
    a
    row
    s0
    s1
    =
    match
      (split_prod ~split0 ~split1 parallel s0 s1 : #((s0 * s1) * (s0 * s1)) Option_u.t)
    with
    | T #(Some, #((s00, s10), (s01, s11))) ->
      Option_u.some #(Cons #(a, ~row, s00, s10), Prod (s01, s11))
    | T #(None, _) -> Option_u.some #(One #(a, ~row), Prod (s0, s1))
  ;;

  let split_product
    (type (s0 : value_or_null mod contended portable)
    (s1 : value_or_null mod contended portable))
    ~split0
    ~split1
    =
    ();
    fun parallel t ->
      match t with
      | Cons #(a, ~row, s0, s1) -> split_cons ~split0 ~split1 parallel a row s0 s1
      | Prod (s0, s1) ->
        (match
           (split_prod ~split0 ~split1 parallel s0 s1
            : #((s0 * s1) * (s0 * s1)) Option_u.t)
         with
         | T #(Some, #((s00, s10), (s01, s11))) ->
           Option_u.some #(Prod (s00, s10), Prod (s01, s11))
         | T #(None, _) -> Option_u.none ())
      | One #(a, ~row) ->
        (match split1 parallel row with
         | T #(Some, #(row0, row1)) ->
           Option_u.some #(One #(a, ~row:row0), One #(a, ~row:row1))
         | T #(None, _) -> Option_u.none ())
  ;;
end

let product_left seq0 seq1 = exclave_
  match seq0, seq1 with
  | (Known _ as seq0), (Known _ as seq1) -> With_length.product_left seq0 seq1
  | Known seq0, Unknown seq1 ->
    Unknown
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:(With_length.next_opt seq0) ~next1:seq1.next
      ; split =
          Product.split_product ~split0:(With_length.split_opt seq0) ~split1:seq1.split
      }
  | Unknown seq0, Known seq1 ->
    Unknown
      { current = Product.create seq0.current seq1.current
      ; next = Product.next_product ~next0:seq0.next ~next1:(With_length.next_opt seq1)
      ; split =
          Product.split_product ~split0:seq0.split ~split1:(With_length.split_opt seq1)
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
  map seq ~f:(fun _ (b, a) : ('a * 'b) -> a, b) [@nontail]
;;

let concat_map t ~f = exclave_ concat (map t ~f)

let fold parallel t ~init ~f ~combine =
  match t with
  | Unknown { current; next; split } ->
    parallel_fold parallel ~init ~f ~state:current ~next ~split ~combine [@nontail]
  | Known _ as t -> With_length.fold parallel t ~init ~f ~combine
;;

let iter parallel t ~f =
  fold
    parallel
    t
    ~init:(fun () -> ())
    ~f:(fun parallel () a -> f parallel a)
    ~combine:(fun _ () () -> ()) [@nontail]
;;

let reduce parallel t ~f =
  fold
    parallel
    t
    ~init:(fun () -> None)
    ~f:(fun parallel acc a ->
      match acc with
      | Some acc -> Some (f parallel acc a)
      | None -> Some a)
    ~combine:(fun parallel a b ->
      Option.merge ~f:(fun a b -> f parallel a b) a b [@nontail])
;;

let find parallel t ~f =
  fold
    parallel
    t
    ~init:(fun () -> None)
    ~f:(fun parallel acc a ->
      match acc with
      | Some _ -> acc
      | None -> if f parallel a then Some a else None)
    ~combine:(fun _ a b -> Option.first_some a b) [@nontail]
;;

let to_list_rev parallel t =
  match t with
  | Unknown { current; next; split } ->
    parallel_fold
      parallel
      ~init:(fun () -> [])
      ~f:(fun _ acc a -> a :: acc)
      ~state:current
      ~next
      ~split
      ~combine:(fun _ a b -> b @ a)
  | Known _ as t -> With_length.to_list_rev parallel t
;;

let to_list parallel t = to_list_rev parallel t |> List.rev

let to_iarray parallel t =
  match t with
  | Unknown _ -> to_list_rev parallel t |> Iarray.of_list_rev
  | Known _ as t -> With_length.to_iarray parallel t
;;
