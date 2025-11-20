open! Base
open Await
open Parallel_kernel
module Atomic = Portable.Atomic
module Sequential = Scheduler.Sequential
module Scheduler = For_scheduler
module Result = Scheduler.Result.Capsule

(* Given N total domains, queue 0 is used by the initial domain and queues 1..N-1 are used
   by worker domains. *)
type t =
  | Single_domain of
      { sequential : Sequential.t
      ; threads : int Atomic.t
      }
  | Multi_domain of
      { queues : Work_deqs.t
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
  | Multi_domain { queues; stop; stopped } ->
    Work_deqs.work queues ~break:(fun () -> true);
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
  if domains < 1 then invalid_arg "Parallel_scheduler.create";
  match domains with
  | 1 -> Single_domain { sequential = Sequential.create (); threads = Atomic.make 0 }
  | _ ->
    let stop_flag = Atomic.make false in
    let queues = Work_deqs.create ~domains in
    let stopped = Countdown_latch.create 1 in
    let t = Multi_domain { queues; stop = stop_flag; stopped } in
    (* [create] is [nonportable], so we are on domain 0. *)
    for idx = 1 to domains - 1 do
      Countdown_latch.incr stopped;
      match
        Multicore.spawn_on
          ~domain:idx
          (fun () ->
            Work_deqs.work queues ~break:(fun () -> Atomic.get stop_flag);
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

let parallel t ~f =
  if is_stopped t then failwith "The scheduler is already stopped";
  match t with
  | Single_domain { sequential; _ } -> Sequential.parallel sequential ~f
  | Multi_domain { queues; _ } ->
    let promote job = Work_deqs.push queues job in
    let wake ~n = Work_deqs.try_wake queues ~n in
    let result = Mvar.create () in
    let root =
      Scheduler.root_exn ~promote ~wake (fun parallel ->
        let res = Result.try_with (fun () -> f parallel) in
        Mvar.put_exn result { many = Result.globalize res };
        Work_deqs.wake queues ~idx:0)
    in
    Work_deqs.push queues root;
    Scheduler.with_heartbeat (fun () ->
      Work_deqs.work queues ~break:(fun () -> Mvar.is_full result) [@nontail]);
    Await_blocking.with_await Terminator.never ~f:(fun await ->
      Result.unwrap_ok_exn (Mvar.take await result).many)
    [@nontail]
;;

module Spawn = struct
  type t =
    { create : Await.t @ local -> Parallel_kernel.t Concurrent.t @ local portable
      @@ global
    ; spawn : 'r 'a. ('r, 'a, Parallel_kernel.t) Concurrent.spawn_fn @@ global
    }

  let thread ~threads =
    let spawn_thread r f ~threads =
      Atomic.incr threads;
      match
        (* [create] is [nonportable], so we are on domain 0. *)
        Multicore.spawn_on
          ~domain:0
          (fun r ->
            Exn.protect
              ~f:(fun () ->
                let scheduler = Sequential.create () in
                Sequential.parallel scheduler ~f:(fun c -> f c r))
              ~finally:(fun () -> Atomic.decr threads) [@nontail])
          r
      with
      | Spawned -> Concurrent.Spawned
      | Failed (r, exn, bt) ->
        Atomic.decr threads;
        Failed (r, exn, bt)
    in
    let rec spawn : type r a. (r, a, Parallel_kernel.t) Concurrent.spawn_fn =
      fun scope ~f r ->
      let token = Scope.add scope in
      spawn_thread ~threads r (fun parallel r ->
        Scope.Token.use token ~f:(fun [@inline] terminator scope ->
          with_concurrent terminator ~f:(fun [@inline] c -> f scope parallel c r)
          [@nontail])
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

  let fiber ~queues =
    let spawn_fiber r f ~queues =
      let promote job = Work_deqs.push queues job in
      let wake ~n = Work_deqs.try_wake queues ~n in
      (* SAFETY: [r] is either consumed by [f] or returned via [Failed]. *)
      let r = (Obj.magic_many [@mode contended portable unique]) r in
      let f parallel =
        let r = (Obj.magic_unique [@mode contended portable]) r in
        f parallel r
      in
      match Scheduler.root_exn f ~promote ~wake with
      | root ->
        Work_deqs.push queues root;
        Work_deqs.wake_one queues;
        Concurrent.Spawned
      | exception (Scheduler.Out_of_fibers as exn) ->
        (* SAFETY: see above *)
        let r = (Obj.magic_unique [@mode contended portable]) r in
        let bt = Backtrace.Exn.most_recent () in
        Concurrent.Failed (r, exn, bt)
    in
    let rec spawn : type r a. (r, a, Parallel_kernel.t) Concurrent.spawn_fn =
      fun scope ~f r ->
      let token = Scope.add scope in
      spawn_fiber ~queues r (fun parallel r ->
        Scope.Token.use token ~f:(fun [@inline] terminator scope ->
          with_concurrent parallel terminator ~f:(fun [@inline] c -> f scope parallel c r)
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
  | Multi_domain { queues; _ } ->
    let%tydi { create; _ } = Spawn.fiber ~queues in
    parallel t ~f:(fun parallel ->
      Await.with_ parallel ~terminator ~yield:Null ~await:Scheduler.await ~f:(fun await ->
        f (create await) [@nontail]))
;;

module Expert = struct
  let scheduler t =
    match t with
    | Single_domain { threads; _ } ->
      let%tydi { spawn; _ } = Spawn.thread ~threads in
      Concurrent.Scheduler.create ~spawn:(fun scope ~f r ->
        if is_stopped t then failwith "The scheduler is already stopped";
        spawn scope ~f r)
    | Multi_domain { queues; _ } ->
      (* Each task must request heartbeats since they do not have an outer scope. *)
      let%tydi { spawn; _ } = Spawn.fiber ~queues in
      Concurrent.Scheduler.create ~spawn:(fun scope ~f r ->
        if is_stopped t then failwith "The scheduler is already stopped";
        spawn
          scope
          ~f:(fun scope ctx concurrent r ->
            Scheduler.with_heartbeat (fun () -> f scope ctx concurrent r [@nontail])
            [@nontail])
          r)
  ;;
end
