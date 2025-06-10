open! Base

module Option : sig @@ portable
  include module type of Option

  val first_some_portable_contended
    :  'a t @ contended portable
    -> 'a t @ contended portable
    -> 'a t @ contended portable

  val merge_portable_contended
    :  'a t @ contended portable
    -> 'a t @ contended portable
    -> f:('a @ contended portable -> 'a @ contended portable -> 'a @ contended portable)
       @ local portable
    -> 'a t @ contended portable
end = struct
  include Option

  let first_some_portable_contended x y =
    match x with
    | Some _ -> x
    | None -> y
  ;;

  let merge_portable_contended a b ~f =
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (f a b)
  ;;
end

module List : sig @@ portable
  include module type of List

  val ( @ )
    :  'a t @ contended portable
    -> 'a t @ contended portable
    -> 'a t @ contended portable

  val length : 'a t @ contended -> int
  val rev_portable_contended : 'a t @ contended portable -> 'a t @ contended portable

  val split_portable_contended
    :  'a t @ contended portable
    -> n:int
    -> 'a t * 'a t @ contended portable
end = struct
  include List

  let[@tail_mod_cons] rec ( @ ) l1 l2 =
    match l1 with
    | [] -> l2
    | hd :: tl -> hd :: (tl @ l2)
  ;;

  let rec length_aux len = function
    | [] -> len
    | _ :: l -> length_aux (len + 1) l
  ;;

  let length l = length_aux 0 l

  let rec rev_append_portable_contended l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> rev_append_portable_contended l (a :: l2)
  ;;

  let rev_portable_contended l = rev_append_portable_contended l []

  (* Copied from [Base]. *)
  let split_portable_contended l ~n =
    if n <= 0
    then [], l
    else (
      let rec loop n t accum =
        match t with
        | [] -> l, [] (* in this case, l = rev accum *)
        | hd :: tl ->
          if n = 0
          then rev_append_portable_contended accum [], t
          else loop (n - 1) tl (hd :: accum)
      in
      loop n l [])
  ;;
end

module Array : sig @@ portable
  include module type of Array

  val empty : unit -> 'a array @ portable

  external racy_set_contended
    :  ('a t[@local_opt]) @ contended
    -> int
    -> 'a @ contended
    -> unit
    = "%array_safe_set"

  external make_portable_contended
    :  int
    -> 'a @ contended portable
    -> 'a t @ contended portable
    = "caml_make_vect"
end = struct
  include Array

  let empty () = Obj.magic_portable [||]

  external racy_set_contended
    :  ('a t[@local_opt]) @ contended
    -> int
    -> 'a @ contended
    -> unit
    @@ portable
    = "%array_safe_set"

  external make_portable_contended
    :  int
    -> 'a @ contended portable
    -> 'a t @ contended portable
    @@ portable
    = "caml_make_vect"
end

module Iarray : sig @@ portable
  include module type of Iarray

  external length : ('a t[@local_opt]) @ contended -> int @@ portable = "%array_length"

  external racy_get_portable_contended
    :  ('a t[@local_opt]) @ contended portable
    -> int
    -> ('a[@local_opt]) @ contended portable
    = "%array_safe_get"

  external unsafe_of_array__promise_no_mutation
    : 'a.
    ('a array[@local_opt]) @ contended portable -> ('a t[@local_opt]) @ contended portable
    = "%array_to_iarray"
end = struct
  include Iarray

  external length : ('a t[@local_opt]) @ contended -> int @@ portable = "%array_length"

  external racy_get_portable_contended
    :  ('a t[@local_opt]) @ contended portable
    -> int
    -> ('a[@local_opt]) @ contended portable
    @@ portable
    = "%array_safe_get"

  external unsafe_of_array__promise_no_mutation
    : 'a.
    ('a array[@local_opt]) @ contended portable -> ('a t[@local_opt]) @ contended portable
    @@ portable
    = "%array_to_iarray"
end
