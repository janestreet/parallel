@@ portable

open! Base
open! Import

module Incident : sig
  (** [t] is the type of panic-inducing incidents. *)
  type t : value mod contended portable

  (** [create_uncaught m e b] creates a fresh incident representing the uncaught exception
      [e] with backtrace [b] in capsule [m]. *)
  val create_uncaught
    :  'k Capsule.Mutex.t
    -> (exn, 'k) Capsule.Data.t
    -> (Backtrace.t, 'k) Capsule.Data.t
    -> t

  (** [equal t1 t2] is [true] if [t1] and [t2] are the same incident and [false]
      otherwise. *)
  val equal : [%equal: t]

  (** [sexp_of_t t] is a sexp describing the exception that caused [t] and its backtrace. *)
  val sexp_of_t : [%sexp_of: t]

  (** [to_string t] is a machine-readable string describing the exception that caused [t]
      and its backtrace. *)
  val to_string : t -> string

  (** [to_string_hum t] is a human-readable string describing the exception that caused
      [t] and its backtrace. *)
  val to_string_hum : t -> string
end

(** [t] is the type of panics. It includes a description of the incident (or incidents)
    that caused the panic and the context in which that incident occurred. *)
type t : value mod contended portable

(** [Panic p] is an exception indicating that a panic [p] has occurred. *)
exception Panic of t
[@@deriving sexp_of]

module Monitor : sig
  (** [t] is the type of monitors that will be notified when some tasks panic. *)
  type t : value mod contended portable

  (** [create ~parent] creates a fresh monitor with [parent] as its parent. If the
      resulting monitor is notified of a panic then [parent] will also be notified. *)
  val create : parent:t -> t

  (** [create_root t] creates a fresh monitor with no parent. *)
  val create_root : unit -> t

  (** [panic t p] notifies [t] that the incident [p] has occured. *)
  val panic : t -> Incident.t -> unit

  (** [on_panic t f] registers [f] as an incident handler on [t]. [f] will be run whenever
      [t] is notified of an incident. If [f] raises an uncaught exception, the program is
      terminated. It is not possible to handle such exceptions, as they may have nothing
      to do with the current execution context. *)
  val on_panic : t -> (Incident.t -> unit) @ portable unyielding -> unit
end

(** [of_incident i] represents a panic caused by [i]. *)
val of_incident : Incident.t -> t

(** [with_backtrace t b] is a panic with the same causes as [t] but whose context has been
    extended with the backtrace [b]. *)
val with_backtrace : t -> 'k Capsule.Mutex.t -> (Backtrace.t, 'k) Capsule.Data.t -> t

(** [join ts] is a panic representing the combination of [ts] that come from separate
    concurrent or parallel tasks. *)
val join : t list -> t

(** [sexp_of_t t] is a sexp describing the incident(s) that caused [t] and its backtrace. *)
val sexp_of_t : [%sexp_of: t]

(** [to_string t] is a machine-readable string describing the incident(s) that caused [t]
    and its backtrace. *)
val to_string : t -> string

(** [to_string_hum t] is a human-readable string describing the incident(s) that caused
    [t] and its backtrace. *)
val to_string_hum : t -> string

(** [iter_incidents t f] allows [f] to examine all incidents contained in this panic. *)
val iter_incidents : t -> (Incident.t -> unit) @ local -> unit

(** [handle_panics_and_report_exceptions monitor ~on_panic ~f] runs [f], reporting
    uncaught exceptions to [monitor] and top-level panics to [on_panic].

    If [f] raises [Panic], [handle] applies [on_panic] to the panic augmented with the
    current backtrace. [monitor] is not signaled.

    If [f] raises any other exception, [handle] creates a fresh incident containing the
    exception and reports it to [monitor]. [handle] then applies [on_panic] to a fresh
    [Panic] created from the incident.

    If [on_panic] raises an uncaught exception, the program is terminated. It is not
    possible to handle such exceptions, as they may have nothing to do with the current
    execution context. *)
val handle_panics_and_report_exceptions
  :  Monitor.t
  -> on_panic:(t -> 'a) @ local once portable
  -> f:(unit -> 'a) @ local once portable
  -> 'a

module Result : sig
  type panic := t

  type 'a t : value mod contended portable =
    | Ok : ('a, 'k) Capsule.Data.t @@ aliased global * 'k Capsule.Key.t @@ global -> 'a t
    | Panic of panic @@ aliased global
  [@@unsafe_allow_any_mode_crossing]

  (** [handle_panics_and_report_exceptions monitor f] runs [f], reporting uncaught
      exceptions to [monitor].

      If [f] raises [Panic], [handle] returns [Panic] containing the panic augmented with
      the current backtrace. [monitor] is not signaled.

      If [f] raises any other exception, [handle] creates a fresh incident containing the
      exception and reports it to [monitor]. [handle] then returns [Panic] containing a
      fresh panic created from the incident. *)
  val handle_panics_and_report_exceptions
    :  Monitor.t
    -> (unit -> 'a) @ local once portable
    -> 'a t @ local unique

  val globalize : 'a t @ local unique -> 'a t @ unique
end
