@@ portable

open! Base

type 'a t =
  | Ok of 'a @@ global
  | Exn of Exn.t @@ global * Backtrace.t @@ global

(** [try_with f] runs [f], returning [Exn] if [f] raises and [Ok] otherwise. *)
val try_with : (unit -> 'a) @ local once -> 'a t @ local unique

val ok_exn : 'a t @ local -> 'a
val map : 'a t @ local unique -> f:('a -> 'b) @ local once -> 'b t @ local unique
val globalize : 'a t @ local unique -> 'a t @ unique

module Capsule : sig
  module Capsule := Portable.Capsule.Expert

  type 'a t =
    | Ok : ('a, 'k) Capsule.Data.t @@ global many * 'k Capsule.Key.t -> 'a t
    | Exn of Exn.t @@ global many * Backtrace.t @@ global many

  (** [try_with f] runs [f] in a fresh capsule, returning [Exn] if [f] raises and [Ok]
      otherwise. *)
  val try_with : (unit -> 'a) @ local once portable -> 'a t @ local unique

  val unwrap_ok_exn : 'a t @ local unique -> 'a
  val globalize : 'a t @ local unique -> 'a t @ unique
end
