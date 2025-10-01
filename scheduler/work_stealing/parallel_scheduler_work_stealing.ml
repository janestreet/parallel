open! Base
open Await
open Parallel_kernel
module Atomic = Portable.Atomic
module Capsule = Portable.Capsule
module Sequential = Scheduler.Sequential
module Scheduler = For_scheduler
module Result = Scheduler.Result.Capsule
module DLS = Basement.Stdlib_shim.Domain.Safe.DLS

(* Given N total domains, queue 0 is used by the initial domain and queues
   1..N-1 are used by worker domains. *)
type t =
  | Single_domain of
      { sequential : Sequential.t
      ; threads : int Atomic.t
      }
  | Multi_domain of
      { queues : Work_deqs.t
      ; self : Work_deqs.Self.t DLS.key
      ; stop : bool Atomic.t
      ; stopped : Countdown_latch.t
      }

let is_stopped = function
  | Single_domain { sequential; _ } -> Sequential.is_stopped sequential
  | Multi_domain { stop; _ } -> Atomic.get stop
;;

let stop t =
  if is_stopped t then failwith "The scheduler is already stopped";
  match t with
  | Single_domain { sequential; threads } ->
    while Atomic.get threads > 0 do
      Thread.yield ()
    done;
    Sequential.stop sequential
  | Multi_domain { queues; self; stop; stopped } ->
    (* See [create] *)
    let self = Obj.magic_uncontended (DLS.get self) in
    Work_deqs.work queues ~self ~idx:0 ~break:(fun () -> true);
    Atomic.set stop true;
    for i = 1 to Work_deqs.length queues - 1 do
      Work_deqs.wake queues ~idx:i
    done;
    Await_blocking.with_await Terminator.never ~f:(fun await ->
      Countdown_latch.decr stopped;
      Countdown_latch.await await stopped)
    [@nontail]
;;

let create ?max_domains () =
  let domains =
    let default = Multicore.max_domains () in
    Option.value_map max_domains ~f:(Int.min default) ~default
  in
  if domains < 1 then invalid_arg "Parallel_scheduler_work_stealing.create";
  match domains with
  | 1 -> Single_domain { sequential = Sequential.create (); threads = Atomic.make 0 }
  | _ ->
    let stop_flag = Atomic.make false in
    let owners, queues = Work_deqs.create ~domains in
    let owner idx =
      let { many = owner } = Unique.Once.Atomic.get_exn (Iarray.get owners idx) in
      Capsule.Isolated.unwrap owner
    in
    let self =
      (* Usage of this key is thread-safe because only one thread per domain has access to
         the key, we do not yield while accessing the queue, and we do not borrow queues. *)
      DLS.new_key (fun _ -> owner (Multicore.current_domain ()))
    in
    let stopped = Countdown_latch.create 1 in
    let t = Multi_domain { queues; self; stop = stop_flag; stopped } in
    (* [create] is [nonportable], so we are on domain 0. *)
    for idx = 1 to domains - 1 do
      Countdown_latch.incr stopped;
      match
        Multicore.spawn_on
          ~domain:idx
          (fun () ->
            (* See [create] *)
            let self = Obj.magic_uncontended (DLS.get self) in
            Work_deqs.work queues ~self ~idx ~break:(fun () -> Atomic.get stop_flag);
            Countdown_latch.decr stopped)
          ()
      with
      | Spawned ->
        ( (* The new thread is now responsible for decrementing the [stopped] latch. *) )
      | Failed ((), exn, bt) ->
        Countdown_latch.decr stopped;
        stop t;
        Exn.raise_with_original_backtrace exn bt
    done;
    t
;;

let promote ~self job =
  (* See [create] *)
  let self = Obj.magic_uncontended (DLS.get self) in
  Work_deqs.Self.push self job
;;

let parallel t ~f =
  if is_stopped t then failwith "The scheduler is already stopped";
  match t with
  | Single_domain { sequential; _ } -> Sequential.parallel sequential ~f
  | Multi_domain { queues; self; _ } ->
    let promote job = promote ~self job in
    let wake ~n = Work_deqs.try_wake queues ~n in
    let result = Mvar.create () in
    let root =
      Scheduler.root ~promote ~wake (fun parallel ->
        let res = Result.try_with (fun () -> f parallel) in
        Mvar.put_exn result { many = Result.globalize res };
        Work_deqs.wake queues ~idx:0)
    in
    (* See [create] *)
    let self = Obj.magic_uncontended (DLS.get self) in
    Work_deqs.Self.push self root;
    Scheduler.with_heartbeat (fun () ->
      Work_deqs.work queues ~self ~idx:0 ~break:(fun () -> Mvar.is_full result) [@nontail]);
    Await_blocking.with_await Terminator.never ~f:(fun await ->
      Result.unwrap_ok_exn (Mvar.take await result).many)
    [@nontail]
;;

module Spawn = struct
  type t =
    { create : Await.t @ local -> Parallel_kernel.t Concurrent.t @ local portable
      @@ global
    ; spawn : 'a. ('a, Parallel_kernel.t) Concurrent.spawn_fn @@ global
    }

  let thread ~threads =
    let spawn_thread f ~threads =
      Atomic.incr threads;
      match
        (* [create] is [nonportable], so we are on domain 0. *)
        Multicore.spawn_on
          ~domain:0
          (fun () ->
            let scheduler = Sequential.create () in
            Sequential.parallel scheduler ~f;
            Atomic.decr threads)
          ()
      with
      | Spawned -> ()
      | Failed ((), exn, bt) ->
        Atomic.decr threads;
        Exn.raise_with_original_backtrace exn bt
    in
    let rec spawn : type a. (a, Parallel_kernel.t) Concurrent.spawn_fn =
      fun scope ~f ->
      let token = Scope.add scope in
      spawn_thread ~threads (fun parallel ->
        Scope.Token.use token ~f:(fun [@inline] terminator scope ->
          with_concurrent terminator ~f:(fun [@inline] c -> f scope parallel c) [@nontail])
        [@nontail])
    and create await = exclave_
      (Concurrent.create [@mode portable])
        await
        ~scheduler:((Concurrent.Scheduler.create [@mode portable] [@alloc stack]) ~spawn)
    and with_concurrent terminator ~f =
      Await_blocking.with_await terminator ~f:(fun await -> f (create await) [@nontail])
      [@nontail]
    in
    exclave_ { create; spawn }
  ;;

  let fiber ~self ~queues =
    let spawn_fiber f ~self ~queues =
      let promote job = promote ~self job in
      let wake ~n = Work_deqs.try_wake queues ~n in
      let root = Scheduler.root f ~promote ~wake in
      (* See [create] *)
      let self = Obj.magic_uncontended (DLS.get self) in
      Work_deqs.Self.push self root;
      Work_deqs.wake_one queues
    in
    let rec spawn : type a. (a, Parallel_kernel.t) Concurrent.spawn_fn =
      fun scope ~f ->
      let token = Scope.add scope in
      spawn_fiber ~self ~queues (fun parallel ->
        Scope.Token.use token ~f:(fun [@inline] terminator scope ->
          with_concurrent parallel terminator ~f:(fun [@inline] c -> f scope parallel c)
          [@nontail])
        [@nontail])
    and create await = exclave_
      (Concurrent.create [@mode portable])
        await
        ~scheduler:((Concurrent.Scheduler.create [@mode portable] [@alloc stack]) ~spawn)
    and with_concurrent parallel terminator ~f =
      Await.with_ parallel ~terminator ~yield:Null ~await:Scheduler.await ~f:(fun await ->
        f (create await) [@nontail])
      [@nontail]
    in
    exclave_ { create; spawn }
  ;;
end

let concurrent t ~terminator ~f =
  if is_stopped t then failwith "The scheduler is already stopped";
  let terminator = Terminator.Expert.globalize terminator in
  match t with
  | Single_domain { threads; _ } ->
    let%tydi { create; _ } = Spawn.thread ~threads in
    parallel t ~f:(fun _ ->
      Await_blocking.with_await terminator ~f:(fun await -> f (create await) [@nontail]))
  | Multi_domain { queues; self; _ } ->
    let%tydi { create; _ } = Spawn.fiber ~queues ~self in
    parallel t ~f:(fun parallel ->
      Await.with_ parallel ~terminator ~yield:Null ~await:Scheduler.await ~f:(fun await ->
        f (create await) [@nontail]))
;;

module Expert = struct
  let scheduler t =
    match t with
    | Single_domain { threads; _ } ->
      let%tydi { spawn; _ } = Spawn.thread ~threads in
      Concurrent.Scheduler.create ~spawn:(fun scope ~f ->
        if is_stopped t then failwith "The scheduler is already stopped";
        spawn scope ~f)
    | Multi_domain { queues; self; _ } ->
      (* Each task must request heartbeats since they do not have an outer scope. *)
      let%tydi { spawn; _ } = Spawn.fiber ~self ~queues in
      Concurrent.Scheduler.create ~spawn:(fun scope ~f ->
        if is_stopped t then failwith "The scheduler is already stopped";
        spawn scope ~f:(fun scope ctx concurrent ->
          Scheduler.with_heartbeat (fun () -> f scope ctx concurrent [@nontail])
          [@nontail]))
  ;;
end
