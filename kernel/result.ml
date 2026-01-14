open! Base

type 'a t =
  | Ok of 'a @@ global
  | Exn of Exn.t @@ global * Backtrace.t @@ global

let[@inline] try_with f = exclave_
  try Ok (f ()) with
  | exn -> Exn (exn, Backtrace.Exn.most_recent ())
;;

let[@inline] ok_exn = function
  | Ok a -> a
  | Exn (exn, bt) -> Exn.raise_with_original_backtrace exn bt
;;

let[@inline] map t ~f =
  match t with
  | Ok a -> exclave_ Ok (f a)
  | Exn _ as t -> t
;;

let[@inline] globalize = function
  | Ok a -> Ok a
  | Exn (exn, bt) -> Exn (exn, bt)
;;

module Capsule = struct
  module Capsule = Portable.Capsule.Expert

  type 'a t =
    | Ok : ('a, 'k) Capsule.Data.t @@ global many * 'k Capsule.Key.t -> 'a t
    | Exn of Exn.t @@ global many * Backtrace.t @@ global many

  let[@inline] try_with f = exclave_
    let (P key) = Capsule.create () in
    try
      let #(result, key) =
        Capsule.Key.access_local key ~f:(fun [@inline] access ->
          { global = { many = Capsule.Data.wrap ~access (f ()) } })
      in
      Ok (result.global.many, key)
    with
    | exn -> Exn (exn, Backtrace.Exn.most_recent ())
  ;;

  let[@inline] unwrap_ok_exn = function
    | Ok (a, key) -> Capsule.(Data.unwrap ~access:(Key.destroy key) a)
    | Exn (exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline] globalize : 'a t @ local unique -> 'a t @ unique = function
    | Ok (a, key) -> Ok (a, Capsule.Key.globalize_unique key)
    | Exn (exn, bt) -> Exn (exn, bt)
  ;;
end
