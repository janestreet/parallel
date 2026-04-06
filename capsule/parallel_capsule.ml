open! Base
open Await
module Capsule = Await_capsule

(* SAFETY: We explicitly allow passing the Parallel.t capability into callbacks to locks
   because our parallelism is well-structured, preventing deadlocks even if you do
   parallelism while a lock is held *)
external magic_unyielding_parallel
  :  Parallel_kernel.t @ local
  -> Parallel_kernel.t @ local unyielding
  @@ portable
  = "%identity"

module Mutex = struct
  type 'k t = 'k Sync.Mutex.t

  let with_lock parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Mutex.with_lock (Parallel_kernel.sync parallel) t ~f:(fun _ access ->
      f parallel access)
    [@nontail]
  ;;

  let with_lock_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Mutex.with_lock_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ access -> f parallel access)
    [@nontail]
  ;;

  module Poisoning = struct
    let with_lock parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Mutex.Poisoning.with_lock
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;

    let with_lock_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Mutex.Poisoning.with_lock_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;
  end

  module Expert = struct
    let with_access parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_access (Parallel_kernel.sync parallel) t ~f:(fun _sync access ->
        f parallel access)
      [@nontail]
    ;;

    let with_access_poisoning parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_access_poisoning
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_access_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_or_cancel_poisoning parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_access_or_cancel_poisoning
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_password parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_password (Parallel_kernel.sync parallel) t ~f:(fun _sync password ->
        f parallel password)
      [@nontail]
    ;;

    let with_password_poisoning parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_password_poisoning
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_password_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_or_cancel_poisoning parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Mutex.with_password_or_cancel_poisoning
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    [%%template
    [@@@alloc.default a @ l = (heap_global, stack_local)]

    let with_key parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Mutex.with_key [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_poisoning parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Mutex.with_key_poisoning [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_or_cancel parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Mutex.with_key_or_cancel [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_or_cancel_poisoning parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Mutex.with_key_or_cancel_poisoning [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;]
  end
end

module With_mutex = struct
  type 'a t = 'a Capsule.Sync.With_mutex.t

  let with_lock parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_mutex.with_lock (Parallel_kernel.sync parallel) t ~f:(fun _ value ->
      f parallel value)
    [@nontail]
  ;;

  let with_lock_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_mutex.with_lock_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ value -> f parallel value)
    [@nontail]
  ;;

  let with_guard parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_mutex.with_guard
      (Parallel_kernel.sync parallel)
      t
      ~f:(fun _ guard -> f parallel guard)
    [@nontail]
  ;;

  let with_guard_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_mutex.with_guard_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ guard -> f parallel guard)
    [@nontail]
  ;;

  module Poisoning = struct
    let with_lock parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_mutex.Poisoning.with_lock
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;

    let with_lock_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_mutex.Poisoning.with_lock_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;
  end
end

module Rwlock = struct
  type 'k t = 'k Sync.Rwlock.t

  let with_write parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Rwlock.with_write (Parallel_kernel.sync parallel) t ~f:(fun _ access ->
      f parallel access)
    [@nontail]
  ;;

  let with_read parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Rwlock.with_read (Parallel_kernel.sync parallel) t ~f:(fun _ access ->
      f parallel access)
    [@nontail]
  ;;

  let with_write_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Rwlock.with_write_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ access -> f parallel access)
    [@nontail]
  ;;

  let with_read_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.Rwlock.with_read_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ access -> f parallel access)
    [@nontail]
  ;;

  module Poisoning = struct
    let with_write parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Rwlock.Poisoning.with_write
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;

    let with_write_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Rwlock.Poisoning.with_write_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;

    let with_read parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Rwlock.Poisoning.with_read
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;

    let with_read_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.Rwlock.Poisoning.with_read_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ access -> f parallel access)
      [@nontail]
    ;;
  end

  module Expert = struct
    let with_access_shared parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_shared
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_shared_freezing parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_shared_freezing
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_shared_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_shared_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_shared_or_cancel_freezing parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_shared_or_cancel_freezing
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access (Parallel_kernel.sync parallel) t ~f:(fun _sync access ->
        f parallel access)
      [@nontail]
    ;;

    let with_access_poisoning parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_poisoning
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_access_or_cancel_poisoning parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_access_or_cancel_poisoning
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync access -> f parallel access)
      [@nontail]
    ;;

    let with_password_shared parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_shared
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_shared_freezing parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_shared_freezing
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_shared_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_shared_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_shared_or_cancel_freezing parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_shared_or_cancel_freezing
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_poisoning parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_poisoning
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    let with_password_or_cancel_poisoning parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Sync.Rwlock.with_password_or_cancel_poisoning
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _sync password -> f parallel password)
      [@nontail]
    ;;

    [%%template
    [@@@alloc.default a @ l = (heap_global, stack_local)]

    let with_key_shared parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_shared [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_shared_freezing parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_shared_freezing [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_shared_or_cancel parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_shared_or_cancel [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_shared_or_cancel_freezing parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_shared_or_cancel_freezing [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_poisoning parallel t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_poisoning [@alloc a])
         (Parallel_kernel.sync parallel)
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_or_cancel parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_or_cancel [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;

    let with_key_or_cancel_poisoning parallel cancellation t ~f =
      (let parallel = magic_unyielding_parallel parallel in
       (Sync.Rwlock.with_key_or_cancel_poisoning [@alloc a])
         (Parallel_kernel.sync parallel)
         cancellation
         t
         ~f:(fun _sync key -> f parallel key [@exclave_if_stack a])
       [@nontail])
      [@exclave_if_stack a]
    ;;]
  end
end

module With_rwlock = struct
  type 'a t = 'a Capsule.Sync.With_rwlock.t

  let with_write parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_write
      (Parallel_kernel.sync parallel)
      t
      ~f:(fun _ value -> f parallel value)
    [@nontail]
  ;;

  let with_write_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_write_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ value -> f parallel value)
    [@nontail]
  ;;

  let with_write_guard parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_write_guard
      (Parallel_kernel.sync parallel)
      t
      ~f:(fun _ guard -> f parallel guard)
    [@nontail]
  ;;

  let with_write_guard_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_write_guard_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ guard -> f parallel guard)
    [@nontail]
  ;;

  let with_read parallel t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_read
      (Parallel_kernel.sync parallel)
      t
      ~f:(fun _ value -> f parallel value)
    [@nontail]
  ;;

  let with_read_or_cancel parallel cancellation t ~f =
    let parallel = magic_unyielding_parallel parallel in
    Capsule.Sync.With_rwlock.with_read_or_cancel
      (Parallel_kernel.sync parallel)
      cancellation
      t
      ~f:(fun _ value -> f parallel value)
    [@nontail]
  ;;

  module Poisoning = struct
    let with_write parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_write
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;

    let with_write_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_write_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;

    let with_write_guard parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_write_guard
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ guard -> f parallel guard)
      [@nontail]
    ;;

    let with_write_guard_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_write_guard_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ guard -> f parallel guard)
      [@nontail]
    ;;

    let with_read parallel t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_read
        (Parallel_kernel.sync parallel)
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;

    let with_read_or_cancel parallel cancellation t ~f =
      let parallel = magic_unyielding_parallel parallel in
      Capsule.Sync.With_rwlock.Poisoning.with_read_or_cancel
        (Parallel_kernel.sync parallel)
        cancellation
        t
        ~f:(fun _ value -> f parallel value)
      [@nontail]
    ;;
  end
end
