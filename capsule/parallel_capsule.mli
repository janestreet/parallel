@@ portable

(** Locking primitives that take a [Parallel.t] and provide it to callbacks.

    These are versions of the locking primitives in [Await_capsule.Sync], for use where
    you want to perform parallel work while a lock is held *)

open! Base
open Await

module Mutex : sig
  type 'k t = 'k Capsule.Sync.Mutex.t

  (** [with_lock parallel t ~f] acquires the mutex [t], runs [f] within the associated
      capsule with access to [parallel] for doing parallel work, then releases [t].

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_lock
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'k t
    -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a @ contended portable

  (** [with_lock_or_cancel parallel c t ~f] is [Completed (with_lock parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_lock_or_cancel
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended portable

  (** Functions that poison the mutex if the provided callback raises an exception. *)
  module Poisoning : sig
    (** [with_lock parallel t ~f] acquires the mutex [t], runs [f] within the associated
        capsule with access to [parallel] for doing parallel work, then releases [t].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise.

        @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
    val with_lock
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'k t
      -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a @ contended portable

    (** [with_lock_or_cancel parallel c t ~f] is [Completed (with_lock parallel t ~f)] if
        [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_lock_or_cancel
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t
      -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended portable
  end

  (** Versions of the functions in {!Capsule.Sync.Mutex.Expert} which take a
      [Parallel_kernel.t] instead of an [Await.t]. *)
  module Expert : sig
    val with_access
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_access_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_password
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    val with_password_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    [%%template:
    [@@@mode.default l = (global, local)]

    val with_key_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Key.t @ unique
            -> #('a * 'k Capsule.Prim.Key.t) @ l once unique)
         @ local once unyielding
      -> 'a @ l once unique

    val with_key_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Key.t @ unique
            -> #('a * 'k Capsule.Prim.Key.t) @ l once unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ l once unique]
  end
end

module With_mutex : sig
  type 'a t = 'a Capsule.Sync.With_mutex.t

  (** [with_lock parallel t ~f] locks the mutex associated with [t] and calls [f] on the
      protected value with access to [parallel] for doing parallel work, returning the
      result. *)
  val with_lock
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b @ contended portable

  (** [with_lock_or_cancel parallel c t ~f] is [Completed (with_lock parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_lock_or_cancel
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b Or_canceled.t @ contended portable

  (** [with_scoped parallel t ~f] locks the mutex associated with [t] and calls [f] with
      [parallel] and a [Scoped.t] for the protected data, returning the result.

      Since the provided callback does not have to be [portable], this is useful when you
      need to acquire two mutexes for different capsules at the same time and move data
      between them. For example:

      {[
        open! Core
        open! Await

        (* Lock both mutexes, and set the value of both refs to the maximum of their
           previous values, without any data races

           Make sure to always call this with locks in the same order, to avoid deadlocks! *)
        let set_to_max
          :  Parallel.t @ local -> int ref Capsule.Sync.With_mutex.t
          -> int ref Capsule.Sync.With_mutex.t -> unit
          =
          fun par ref1 ref2 ->
          Parallel.Capsule.With_mutex.with_scoped par ref1 ~f:(fun par scope1 ->
            Parallel.Capsule.With_mutex.with_scoped par ref2 ~f:(fun _par scope2 ->
              (* At this point we have both mutexes locked, so we can freely manipulate
                 the data they protect without the potential for data races *)
              let value1 = Capsule.Scoped.get scope1 ~f:(fun r -> !r) in
              let value2 = Capsule.Scoped.get scope2 ~f:(fun r -> !r) in
              let new_value = Int.max value1 value2 in
              Capsule.Scoped.iter scope1 ~f:(fun r -> r := new_value);
              Capsule.Scoped.iter scope2 ~f:(fun r -> r := new_value) [@nontail])
            [@nontail])
          [@nontail]
        ;;
      ]} *)
  val with_scoped
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
       @ local once unyielding
    -> 'b

  (** [with_scoped_or_cancel parallel c t ~f] is [Completed (with_scoped parallel t ~f)]
      if [c] is not canceled, otherwise it is [Canceled]. *)
  val with_scoped_or_cancel
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
       @ local once unyielding
    -> 'b Or_canceled.t

  (** Functions that poison the mutex if the provided callback raises an exception. *)
  module Poisoning : sig
    (** [with_lock parallel t ~f] locks the mutex associated with [t] and calls [f] on the
        protected value with access to [parallel] for doing parallel work, returning the
        result.

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_lock
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b @ contended portable

    (** [with_lock_or_cancel parallel c t ~f] is [Completed (with_lock parallel t ~f)] if
        [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_lock_or_cancel
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b Or_canceled.t @ contended portable
  end
end

module Rwlock : sig
  type 'k t = 'k Capsule.Sync.Rwlock.t

  (** [with_write parallel t ~f] acquires [t] for writing, runs [f] within the associated
      capsule with access to [parallel] for doing parallel work, then releases [t].

      @raise Poisoned if [t] cannot be acquired because it is poisoned.
      @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
  val with_write
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'k t
    -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a @ contended portable

  (** [with_read parallel t ~f] acquires [t] for reading, runs [f] within the associated
      capsule with access to [parallel] for doing parallel work, then releases [t].

      @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
  val with_read
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'k t
    -> f:
         (Parallel_kernel.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a @ contended portable

  (** [with_write_or_cancel parallel c t ~f] is [Completed (with_write parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_write_or_cancel
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended portable

  (** [with_read_or_cancel parallel c t ~f] is [Completed (with_read parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_read_or_cancel
    : 'k 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'k t
    -> f:
         (Parallel_kernel.t @ local
          -> 'k Capsule.Access.t @ shared
          -> 'a @ contended portable)
       @ local once portable unyielding
    -> 'a Or_canceled.t @ contended portable

  (** Functions that poison or freeze the lock if the provided callback raises an
      exception. *)
  module Poisoning : sig
    (** [with_write parallel t ~f] acquires [t] for writing, runs [f] within the
        associated capsule with access to [parallel] for doing parallel work, then
        releases [t].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise.

        @raise Poisoned if [t] cannot be acquired because it is poisoned.
        @raise Frozen if [t] cannot be acquired for writing because it is frozen. *)
    val with_write
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'k t
      -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a @ contended portable

    (** [with_write_or_cancel parallel c t ~f] is [Completed (with_write parallel t ~f)]
        if [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_write_or_cancel
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t
      -> f:(Parallel_kernel.t @ local -> 'k Capsule.Access.t -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended portable

    (** [with_read parallel t ~f] acquires [t] for reading, runs [f] within the associated
        capsule with access to [parallel] for doing parallel work, then releases [t].

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise.

        @raise Poisoned if [t] cannot be acquired because it is poisoned. *)
    val with_read
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'k t
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Access.t @ shared
            -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a @ contended portable

    (** [with_read_or_cancel parallel c t ~f] is [Completed (with_read parallel t ~f)] if
        [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise. *)
    val with_read_or_cancel
      : 'k 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Access.t @ shared
            -> 'a @ contended portable)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended portable
  end

  (** Versions of the functions in {!Capsule.Sync.Rwlock.Expert} which take a
      [Parallel_kernel.t] instead of an [Await.t]. *)
  module Expert : sig
    val with_access_shared
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t @ shared
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_shared_freezing
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t @ shared
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_shared_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t @ shared
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_access_shared_or_cancel_freezing
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t @ shared
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_access
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a @ contended once portable unique

    val with_access_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_access_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Access.t
            -> 'a @ contended once portable unique)
         @ local once portable unyielding
      -> 'a Or_canceled.t @ contended once portable unique

    val with_password_shared
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.Shared.t @ forkable local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_shared_freezing
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.Shared.t @ forkable local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_shared_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.Shared.t @ forkable local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    val with_password_shared_or_cancel_freezing
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.Shared.t @ forkable local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    val with_password
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a @ unique

    val with_password_or_cancel
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    val with_password_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Password.t @ local
            -> 'a @ unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ unique

    [%%template:
    [@@@mode.default l = (global, local)]

    val with_key_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Key.t @ unique
            -> #('a * 'k Capsule.Prim.Key.t) @ l once unique)
         @ local once unyielding
      -> 'a @ l once unique

    val with_key_or_cancel_poisoning
      : ('a : value_or_null) 'k.
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'k t @ local
      -> f:
           (Parallel_kernel.t @ local
            -> 'k Capsule.Prim.Key.t @ unique
            -> #('a * 'k Capsule.Prim.Key.t) @ l once unique)
         @ local once unyielding
      -> 'a Or_canceled.t @ l once unique]
  end
end

module With_rwlock : sig
  type 'a t = 'a Capsule.Sync.With_rwlock.t

  (** [with_write parallel t ~f] locks the reader-writer lock associated with [t] for
      writing and calls [f] on the protected value with access to [parallel] for doing
      parallel work, returning the result. *)
  val with_write
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b @ contended portable

  (** [with_write_or_cancel parallel c t ~f] is [Completed (with_write parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_write_or_cancel
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b Or_canceled.t @ contended portable

  (** [with_scoped t ~f] locks the reader-writer lock associated with [t] for writing and
      calls [f] with a [Scoped.t] for the protected data, returning the result.

      Since the provided callback does not have to be [portable], this is useful when you
      need to acquire two mutexes for different capsules at the same time and move data
      between them. See {!With_mutex.with_scoped} for an example. *)
  val with_scoped
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
       @ local once unyielding
    -> 'b

  (** [with_scoped_or_cancel parallel c t ~f] is [Completed (with_scoped parallel t ~f)]
      if [c] is not canceled, otherwise it is [Canceled]. *)
  val with_scoped_or_cancel
    : 'a ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
       @ local once unyielding
    -> 'b Or_canceled.t

  (** [with_read parallel t ~f] locks the reader-writer lock associated with [t] for
      reading and calls [f] on the protected value with access to [parallel] for doing
      parallel work, returning the result. *)
  val with_read
    : ('a : value mod portable) ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b @ contended portable

  (** [with_read_or_cancel parallel c t ~f] is [Completed (with_read parallel t ~f)] if
      [c] is not canceled, otherwise it is [Canceled]. *)
  val with_read_or_cancel
    : ('a : value mod portable) ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'b @ contended portable)
       @ local once portable unyielding
    -> 'b Or_canceled.t @ contended portable

  (** [with_scoped_shared t ~f] locks the reader-writer lock associated with [t] for
      reading and calls [f] with a [Scoped.Shared.t] for the protected data, returning the
      result.

      [with_scope_shared] is similar to {!with_scoped}, but hands out a
      {!Capsule.Scoped.Shared.t} *)
  val with_scoped_shared
    : ('a : value mod portable) ('b : value_or_null).
    Parallel_kernel.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.Shared.t @ forkable local -> 'b)
       @ local once unyielding
    -> 'b

  (** [with_scoped_shared_or_cancel parallel c t ~f] is
      [Completed (with_scoped_shared parallel t ~f)] if [c] is not canceled, otherwise it
      is [Canceled]. *)
  val with_scoped_shared_or_cancel
    : ('a : value mod portable) ('b : value_or_null).
    Parallel_kernel.t @ local
    -> Cancellation.t @ local
    -> 'a t
    -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.Shared.t @ forkable local -> 'b)
       @ local once unyielding
    -> 'b Or_canceled.t

  (** Functions that poison or freeze the lock if the provided callback raises an
      exception. *)
  module Poisoning : sig
    (** [with_write parallel t ~f] locks the reader-writer lock associated with [t] for
        writing and calls [f] on the protected value with access to [parallel] for doing
        parallel work, returning the result.

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_write
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b @ contended portable

    (** [with_write_or_cancel parallel c t ~f] is [Completed (with_write parallel t ~f)]
        if [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_write_or_cancel
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b Or_canceled.t @ contended portable

    (** [with_scoped parallel t ~f] locks the reader-writer lock associated with [t] for
        writing and calls [f] with a [Scoped.t] for the protected value, returning the
        result.

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_scoped
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
         @ local once unyielding
      -> 'b

    (** [with_scoped_or_cancel parallel c t ~f] is [Completed (with_scoped parallel t ~f)]
        if [c] is not canceled, or [Canceled] otherwise.

        If [f] raises, [t] will be poisoned, meaning all subsequent attempts to acquire it
        will raise. *)
    val with_scoped_or_cancel
      : 'a ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a Capsule.Scoped.t @ local -> 'b)
         @ local once unyielding
      -> 'b Or_canceled.t

    (** [with_read parallel t ~f] locks the reader-writer lock associated with [t] for
        reading and calls [f] on the protected value with access to [parallel] for doing
        parallel work, returning the result.

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise. *)
    val with_read
      : ('a : value mod portable) ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b @ contended portable

    (** [with_read_or_cancel parallel c t ~f] is [Completed (with_read parallel t ~f)] if
        [c] is not canceled, otherwise it is [Canceled].

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise. *)
    val with_read_or_cancel
      : ('a : value mod portable) ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'a t
      -> f:(Parallel_kernel.t @ local -> 'a @ shared -> 'b @ contended portable)
         @ local once portable unyielding
      -> 'b Or_canceled.t @ contended portable

    (** [with_scoped_shared t ~f] locks the reader-writer lock associated with [t] for
        reading and calls [f] with a [Scoped.Shared.t] for the protected data, returning
        the result.

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise. *)
    val with_scoped_shared
      : ('a : value mod portable) ('b : value_or_null).
      Parallel_kernel.t @ local
      -> 'a t
      -> f:
           (Parallel_kernel.t @ local
            -> 'a Capsule.Scoped.Shared.t @ forkable local
            -> 'b)
         @ local once unyielding
      -> 'b

    (** [with_scoped_shared_or_cancel parallel c t ~f] is
        [Completed (with_scoped_shared parallel t ~f)] if [c] is not canceled, otherwise
        it is [Canceled].

        If [f] raises, [t] will be frozen, meaning all subsequent attempts to acquire it
        for writing will raise. *)
    val with_scoped_shared_or_cancel
      : ('a : value mod portable) ('b : value_or_null).
      Parallel_kernel.t @ local
      -> Cancellation.t @ local
      -> 'a t
      -> f:
           (Parallel_kernel.t @ local
            -> 'a Capsule.Scoped.Shared.t @ forkable local
            -> 'b)
         @ local once unyielding
      -> 'b Or_canceled.t
  end
end
