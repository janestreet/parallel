@@ portable

open! Base
open! Import

(** A cell containing an ['a] that will be filled exactly once. *)
type 'a t : value mod contended portable

(** [create ()] creates a fresh ivar. *)
val create : unit -> 'a t

(** [fill_exn t a] stores [a] in [t] and wakes up all threads waiting on [t]. All future
    [wait t] calls will return immediately. Raises if [t] has already been filled. *)
val fill_exn : 'a t -> 'a @ contended portable unique -> unit

(** [wait t] blocks this thread until [t] has been filled, then returns the filled value. *)
val wait : 'a t -> 'a @ contended portable unique

(** [ready t] returns true if [t] has been filled. *)
val ready : 'a t -> bool
