open! Base
open Parallel_kernel
module Atomic = Portable.Atomic
module Capsule = Portable.Capsule.Expert
module Capsule_with_mutex = Work_deqs.Capsule_with_mutex
module Scheduler = For_scheduler

module Domain = struct
  include Domain
  include Domain.Safe
  include Basement.Stdlib_shim.Domain.Safe
end

module DLS = Domain.DLS

(* For a work-stealing scheduler with N total domains, of the N work queues, the first N-1
   are owned by worker domains, and the Nth by the initial domain. *)
type t =
  { domains : unit Domain.t Iarray.t
  ; queues : Work_deqs.t
  ; self : Work_deqs.Self.t DLS.key
  ; stop : bool Atomic.t
  }

external is_runtime5 : unit -> bool @@ portable = "%runtime5"

let expose (Capsule_with_mutex.P { mutex; data }) =
  let access = Capsule.Mutex.destroy mutex |> Capsule.Key.destroy in
  Capsule.Data.unwrap ~access data
;;

let create ?(domains = Stdlib.Domain.recommended_domain_count ()) () =
  if domains < 1 then invalid_arg "Parallel_scheduler_work_stealing.create";
  match is_runtime5 () with
  | false -> failwith "Parallel_scheduler_work_stealing requires the OCaml 5 runtime."
  | true ->
    let stop = Atomic.make false in
    let owners, (queues : Work_deqs.t) = Work_deqs.create ~domains in
    let self = DLS.new_key (fun _ -> Iarray.get owners (domains - 1) |> expose) in
    let domains =
      Iarray.init (domains - 1) ~f:(fun idx ->
        (Domain.spawn' [@alert "-unsafe_parallelism"]) (fun access ->
          let s = Iarray.get owners idx |> expose in
          DLS.set access self s;
          Work_deqs.work queues ~self:s ~idx ~break:(fun () -> Atomic.get stop) [@nontail]))
    in
    { domains; queues; self; stop }
;;

let stop { domains; queues; self; stop } =
  if Atomic.exchange stop true then failwith "The scheduler is already stopped";
  (* Execute our domain's pending async tasks. *)
  let idx = Iarray.length domains in
  DLS.access (fun access ->
    let self = DLS.get access self in
    Work_deqs.work queues ~self ~idx ~break:(fun () -> true));
  Iarray.iteri domains ~f:(fun idx domain ->
    Work_deqs.wake queues ~idx;
    Domain.join domain)
;;

let promote ~self job =
  DLS.access (fun access ->
    let self = DLS.get access self in
    Work_deqs.Self.push self job)
;;

let schedule { domains; queues; self; stop } ~monitor ~f =
  if Atomic.get stop then failwith "The scheduler is already stopped";
  let result = Scheduler.Ivar.create () in
  let promote job = promote ~self job in
  let wake ~n = Work_deqs.try_wake queues ~n in
  let idx = Iarray.length domains in
  let root =
    Scheduler.root ~promote ~wake ~monitor ~f:(fun parallel ->
      Panic.Result.handle_panics_and_report_exceptions monitor (fun () -> f parallel)
      |> Panic.Result.globalize
      |> Scheduler.Ivar.fill_exn result;
      Work_deqs.wake queues ~idx)
  in
  DLS.access (fun access ->
    let self = DLS.get access self in
    Work_deqs.Self.push self root;
    Work_deqs.work queues ~self ~idx ~break:(fun () -> Scheduler.Ivar.ready result)
    [@nontail]);
  match Scheduler.Ivar.wait result with
  | Ok (a, key) -> Capsule.Data.unwrap ~access:(Capsule.Key.destroy key) a
  | Panic panic -> raise (Panic panic)
;;

module Expert = struct
  let schedule_async { queues; self; stop; _ } ~monitor ~on_panic ~f =
    if Atomic.get stop then failwith "The scheduler is already stopped";
    let promote job = promote ~self job in
    let wake ~n = Work_deqs.try_wake queues ~n in
    let root =
      Scheduler.root ~promote ~wake ~monitor ~f:(fun parallel ->
        Panic.handle_panics_and_report_exceptions monitor ~on_panic ~f:(fun () ->
          f parallel)
        [@nontail])
    in
    DLS.access (fun access ->
      let self = DLS.get access self in
      Work_deqs.Self.push self root);
    Work_deqs.try_wake queues ~n:1
  ;;
end
