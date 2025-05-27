open! Base
open! Import

module type S = sig @@ portable
  type parallel : value mod contended portable
  type t
  type 'k create_fn

  val create : (unit -> t) create_fn

  (** [stop t] waits for pending tasks to complete and joins all worker domains.
      Attempting to schedule new tasks after calling [stop] will raise.

      During [stop], idle workers will not attempt to steal asynchronous tasks. *)
  val stop : t -> unit

  (* $MDX part-begin=schedule *)

  (** [schedule t ~monitor ~f] submits [f] to the scheduler [t] and waits for it to
      complete before returning. If [f] raises an uncaught exception, the incident is
      reported to [monitor]. *)
  val schedule
    :  t
    -> monitor:Panic.Monitor.t
    -> f:(parallel @ local -> 'a) @ once portable unyielding
    -> 'a

  (* $MDX part-end *)
end

module type S_async = sig @@ portable
  include
    S
    with type 'k create_fn :=
      ?domains:int (** default: [Stdlib.Domain.recommended_domain_count ()] *) -> 'k

  module Expert : sig
    (** [schedule_async t ~monitor ~on_panic ~f] submits [f] to the scheduler [t] and
        does *not* wait for it to complete before returning. If [f] panics, the panic is
        reported to [on_panic]. If [f] raises any other uncaught exception, the incident
        is reported to both [monitor] and [on_panic]. If [on_panic] raises an uncaught
        exception, the program is terminated.

        Blocking on an asynchronous task is not supported and will lead to deadlocks.
        However, asynchronous tasks may take locks.

        There is currently no mechanism to limit the creation rate of asynchronous tasks.
        If asynchronous work is generated faster than it can be executed, the scheduler's
        queues will grow unboundedly, leading to resource exhaustion. *)
    val schedule_async
      :  t
      -> monitor:Panic.Monitor.t
      -> on_panic:(Panic.t -> unit) @ once portable unyielding
      -> f:(parallel @ local -> unit) @ once portable unyielding
      -> unit
  end
end
