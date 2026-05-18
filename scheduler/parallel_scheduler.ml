open! Base
open Await
module Scheduler = Parallel_kernel.For_scheduler
module Result = Scheduler.Result

module Spawn = struct
  type t =
    { create : Await.t @ local -> Parallel_kernel.t Concurrent.t @ local portable
      @@ global
    ; spawn :
        ('r : value_or_null) 'a. ('r, 'a, Parallel_kernel.t) Concurrent.Scheduler.spawn_fn
      @@ global
    }

  let thread =
    let rec spawn
      : type (r : value_or_null) a.
        (r, a, Parallel_kernel.t) Concurrent.Scheduler.spawn_fn
      =
      fun scope #{ fn; name; affinity } resource ->
      let task =
        #{ Concurrent.Task.fn =
             (fun scope () concurrent -> exclave_
               fn scope Parallel_kernel.sequential (create (Concurrent.await concurrent)))
         ; name
         ; affinity
         }
      in
      Concurrent_in_thread.scheduler.spawn scope task resource
    and create await = exclave_
      (Concurrent.create [@mode portable])
        await
        ~scheduler:((Concurrent.Scheduler.create [@mode portable] [@alloc stack]) ~spawn)
    in
    { create; spawn }
  ;;

  let inject ~task ~subtask ~try_wake f r =
    (* SAFETY: [r] is either consumed by [f] or returned via [Failed]. *)
    let r = (Obj.magic_many [@mode contended portable unique]) r in
    let f parallel =
      let r = (Obj.magic_unique [@mode contended portable]) r in
      f parallel r
    in
    match Scheduler.root_exn f ~task ~subtask ~try_wake with
    | root ->
      task root;
      Concurrent.Spawned
    | exception (Out_of_fibers as exn) ->
      (* SAFETY: see above *)
      let r = (Obj.magic_unique [@mode contended portable]) r in
      let bt = Backtrace.Exn.most_recent () in
      Concurrent.Failed (r, exn, bt)
  ;;

  let fiber ~queues =
    let task f = Work_deqs.inject queues f in
    let subtask f = Work_deqs.push queues f in
    let try_wake ~n = Work_deqs.try_wake queues ~n in
    let rec spawn
      : type (r : value_or_null) a.
        (r, a, Parallel_kernel.t) Concurrent.Scheduler.spawn_fn
      =
      fun scope #{ fn; affinity = _; name = _ } r ->
      match
        inject
          ~task
          ~subtask
          ~try_wake
          (fun parallel (token, r) ->
            Concurrent.Scope.Token.use token ~f:(fun [@inline] terminator scope ->
              let await = Scheduler.await parallel terminator in
              fn scope parallel (create await) r [@nontail])
            [@nontail])
          (Concurrent.Scope.add scope, r)
      with
      | Spawned -> Spawned
      | Failed ((token, r), exn, bt) ->
        Concurrent.Scope.Token.drop token;
        Failed (r, exn, bt)
    and create await = exclave_
      (Concurrent.create [@mode portable])
        await
        ~scheduler:((Concurrent.Scheduler.create [@mode portable] [@alloc stack]) ~spawn)
    in
    exclave_ { create; spawn }
  ;;
end

let workers ~max_workers =
  let workers =
    let default = Multicore.max_domains () in
    Option.value_map max_workers ~f:(Int.min default) ~default
  in
  if workers < 1 then invalid_arg "Parallel_scheduler: workers < 1";
  workers
;;

let spawn_workers ~workers ~queues ~on_root scope =
  let task i =
    Concurrent.task
      ~name:[%string "Par_sched:%{i#Int}"]
      ~affinity:i
      (fun _ cancellation _ concurrent ->
         assert (Multicore.current_domain () = i);
         Work_deqs.work
           queues
           ~await:(Concurrent.await concurrent)
           ~cancellation [@nontail])
  in
  (Option.iter [@mode local]) on_root ~f:(fun on_root ->
    Concurrent.Scheduler.spawn_daemon' on_root scope (task 0));
  for i = 1 to workers - 1 do
    Concurrent.Scheduler.spawn_daemon' Concurrent_in_thread.scheduler scope (task i)
  done
;;

let with_workers ~workers ~on_root await f =
  let queues = Work_deqs.create ~workers in
  let%tydi { create; _ } = Spawn.fiber ~queues in
  (* When this scope returns, we know all queues are empty, so we may cancel the workers. *)
  (Concurrent.Scope.with_ await () ~f:(fun await scope ->
     spawn_workers ~workers ~queues ~on_root scope;
     let concurrent = create await in
     { many = f concurrent }))
    .many
;;

let with_parallel ?max_workers f =
  (* SAFETY: this is sound because our caller is blocked until [f] completes and [f] does
     not escape its scope. *)
  let f = (Obj.magic_portable [@mode once]) f in
  match workers ~max_workers with
  | 1 -> f Parallel_kernel.sequential
  | workers ->
    (* [f] is [unyielding], so returns in bounded time. Therefore, blocking the current
       thread and becoming unkillable cannot cause deadlocks. *)
    let await =
      (Await.Expert.create [@alloc stack])
        ~sync:Sync.blocking
        ~terminator:Terminator.unkillable
    in
    with_workers
      ~workers
      ~on_root:(Some Concurrent_in_thread.scheduler)
      await
      (fun concurrent ->
         (Concurrent.spawn_join [@mode unique]) concurrent () ~f:(fun _ parallel _ ->
           Result.Capsule.globalize
             (Result.Capsule.try_with (fun () ->
                (Scheduler.with_heartbeat (fun () -> { aliased_many = f parallel }))
                  .aliased_many)) [@nontail])
         [@nontail])
    |> Result.Capsule.unwrap_ok_exn
;;

let with_concurrent ?max_workers f =
  (* [f] is [unyielding], so returns in bounded time. Therefore, blocking the current
     thread and becoming unkillable cannot cause deadlocks. *)
  let await =
    (Await.Expert.create [@alloc stack])
      ~sync:Sync.blocking
      ~terminator:Terminator.unkillable
  in
  match workers ~max_workers with
  | 1 when not (Basement.Stdlib_shim.runtime5 ()) ->
    let%tydi { create; _ } = Spawn.thread in
    f (create await) [@nontail]
  | workers ->
    with_workers
      ~workers
      ~on_root:(Some Concurrent_in_thread.scheduler)
      await
      (fun concurrent ->
         Result.globalize
           (Result.try_with (fun () ->
              (Scheduler.with_heartbeat (fun () -> { aliased_many = f concurrent }))
                .aliased_many)) [@nontail])
    |> Result.ok_exn
;;

let scheduler ?max_workers ?on_root () =
  match workers ~max_workers with
  | 1 when not (Basement.Stdlib_shim.runtime5 ()) ->
    let%tydi { spawn; _ } = Spawn.thread in
    (Concurrent.Scheduler.create [@mode portable]) ~spawn
  | workers ->
    let workers, on_root =
      match workers, on_root with
      | n, None when Multicore.max_domains () > n -> n + 1, None
      | _ -> workers, on_root
    in
    let queues = Work_deqs.create ~workers in
    let%tydi { spawn; _ } = Spawn.fiber ~queues in
    let scope = Concurrent.Scope.Global.create () ~on_exit:(fun _ _ -> ()) in
    (match workers, on_root with
     | 1, None ->
       spawn_workers ~workers ~queues ~on_root:(Some Concurrent_in_thread.scheduler) scope
     | _ -> spawn_workers ~workers ~queues ~on_root scope);
    (Concurrent.Scheduler.create [@mode portable])
      ~spawn:(fun scope #{ fn; affinity; name } r ->
        spawn
          scope
          #{ fn =
               (fun scope ctx concurrent r ->
                 (* We do not have an enclosing scope, so we must request heartbeats. *)
                 Scheduler.with_heartbeat (fun () -> fn scope ctx concurrent r [@nontail])
                 [@nontail])
           ; affinity
           ; name
           }
          r)
;;

let concurrent scheduler await f =
  let concurrent = (Concurrent.create [@mode portable]) await ~scheduler in
  f concurrent [@nontail]
;;

let parallel scheduler f =
  (* SAFETY: this is sound because our caller is blocked until [f] completes and [f] does
     not escape its scope. *)
  let f = (Obj.magic_portable [@mode once]) f in
  (* [f] is [unyielding], so returns in bounded time. Therefore, blocking the current
     thread and becoming unkillable cannot cause deadlocks. *)
  let await =
    (Await.Expert.create [@alloc stack])
      ~sync:Sync.blocking
      ~terminator:Terminator.unkillable
  in
  let concurrent = (Concurrent.create [@mode portable]) await ~scheduler in
  (Concurrent.spawn_join [@mode unique portable])
    concurrent
    ()
    ~f:(fun _ (parallel : Parallel_kernel.t) concurrent ->
      Result.Capsule.globalize
        (Result.Capsule.try_with (fun () -> f parallel concurrent)) [@nontail])
  |> Result.Capsule.unwrap_ok_exn
;;
