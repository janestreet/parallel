open! Base
open Parallel_kernel
module Atomic = Portable.Atomic
module Capsule = Portable.Capsule.Expert
module Scheduler = For_scheduler

type t =
  { domains : unit Domain.t array
  ; queue : Work_stack.t
  ; stop : bool Atomic.t
  }

external is_runtime5 : unit -> bool @@ portable = "%runtime5"

let create ?(domains = Stdlib.Domain.recommended_domain_count ()) () =
  if domains < 1 then invalid_arg "Parallel_scheduler_stack.create";
  let stop = Atomic.make false in
  match is_runtime5 () with
  | false -> failwith "Parallel_scheduler_stack requires the OCaml 5 runtime."
  | true ->
    let queue = Work_stack.create () in
    let domains =
      Array.init (domains - 1) ~f:(fun _ ->
        (Domain.Safe.spawn [@alert "-unsafe_parallelism"]) (fun () ->
          Work_stack.work queue ~break:(fun () -> Atomic.get stop)))
    in
    { domains; queue; stop }
;;

let stop { domains; queue; stop } =
  if Atomic.exchange stop true then failwith "The scheduler is already stopped";
  (* Execute our domain's pending async tasks. *)
  Work_stack.work queue ~break:(fun () -> true);
  Work_stack.wake queue;
  Array.iter domains ~f:Domain.join
;;

let schedule { queue; stop; _ } ~monitor ~f =
  if Atomic.get stop then failwith "The scheduler is already stopped";
  let result = Scheduler.Ivar.create () in
  let promote job = Work_stack.push queue ~f:job in
  let wake ~n:_ = Work_stack.wake queue in
  let root =
    Scheduler.root ~promote ~wake ~monitor ~f:(fun parallel ->
      Panic.Result.handle_panics_and_report_exceptions monitor (fun () -> f parallel)
      |> Panic.Result.globalize
      |> Scheduler.Ivar.fill_exn result;
      Work_stack.wake queue)
  in
  Work_stack.push queue ~f:root;
  Work_stack.work queue ~break:(fun () -> Scheduler.Ivar.ready result);
  match Scheduler.Ivar.wait result with
  | Ok (a, key) -> Capsule.Data.unwrap ~access:(Capsule.Key.destroy key) a
  | Panic panic -> raise (Panic panic)
;;

module Expert = struct
  let schedule_async { queue; stop; _ } ~monitor ~on_panic ~f =
    if Atomic.get stop then failwith "The scheduler is already stopped";
    let promote job = Work_stack.push queue ~f:job in
    let wake ~n:_ = Work_stack.wake queue in
    let root =
      Scheduler.root ~promote ~wake ~monitor ~f:(fun parallel ->
        Panic.handle_panics_and_report_exceptions monitor ~on_panic ~f:(fun () ->
          f parallel)
        [@nontail])
    in
    Work_stack.push queue ~f:root;
    Work_stack.wake queue
  ;;
end
