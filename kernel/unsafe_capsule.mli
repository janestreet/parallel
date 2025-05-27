@@ portable

open! Base
open! Import
open Capsule

(** Equivalent to [access_local], but it is unsafe for [f] to raise.

    This function does not install a handler around [f] that converts uncaught exceptions
    into [Capsule.Encapsulated] exceptions. That means [f] could violate capsule safety by
    raising a nonportable exception that contains mutable state associated with ['k]. *)
val access_local__promise_no_exn
  :  password:'k Password.t @ local
  -> f:('k Access.t -> 'a @ contended local portable unique) @ local once portable
  -> 'a @ contended local portable unique

module Data : sig
  (** Equivalent to [Data.Local.iter], but it is unsafe for [f] to raise.

      This function does not install a handler around [f] that converts uncaught
      exceptions into [Capsule.Encapsulated] exceptions. That means [f] could violate
      capsule safety by raising a nonportable exception that contains mutable state
      associated with ['k]. *)
  val iter_local__promise_no_exn
    :  ('a, 'k) Data.t @ local
    -> password:'k Password.t @ local
    -> f:('a @ local -> unit) @ local once portable
    -> unit

  (** Equivalent to [Data.Local.extract], but it is unsafe for [f] to raise.

      This function does not install a handler around [f] that converts uncaught
      exceptions into [Capsule.Encapsulated] exceptions. That means [f] could violate
      capsule safety by raising a nonportable exception that contains mutable state
      associated with ['k]. *)
  val extract__promise_no_exn
    :  ('a, 'k) Data.t @ local
    -> password:'k Password.t @ local
    -> f:('a @ local -> 'b @ contended portable unique) @ local once portable
    -> 'b @ contended portable unique
end
