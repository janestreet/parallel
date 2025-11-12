@@ portable

open! Base

type 'a t =
  | Ok of 'a @@ aliased global
  | Exn of Exn.t @@ aliased global * Backtrace.t @@ aliased global

(** [try_with f] runs [f], returning [Exn] if [f] raises and [Ok] otherwise. *)
val try_with : (unit -> 'a) @ local once -> 'a t @ local unique

val ok_exn : 'a t @ local -> 'a
val map : 'a t @ local unique -> f:('a -> 'b) @ local once -> 'b t @ local unique
val globalize : 'a t @ local unique -> 'a t @ unique

module Capsule : sig
  module Capsule := Portable.Capsule.Expert

  type%fuelproof 'a t : value mod contended forkable many portable unyielding =
    | Ok :
        ('a, 'k) Capsule.Data.t @@ aliased forkable global many unyielding
        * 'k Capsule.Key.t @@ global
        -> 'a t
    | Exn of
        Exn.t @@ aliased forkable global many unyielding
        * Backtrace.t @@ aliased global many
  [@@allow_redundant_modalities]

  (** [try_with f] runs [f] in a fresh capsule, returning [Exn] if [f] raises and [Ok]
      otherwise. *)
  val try_with : (unit -> 'a) @ local once portable -> 'a t @ local unique

  val unwrap_ok_exn : 'a t @ local unique -> 'a
  val globalize : 'a t @ local unique -> 'a t @ unique
end
