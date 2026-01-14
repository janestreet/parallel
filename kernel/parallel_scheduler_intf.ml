open! Base
open! Import

module type S = sig
  type parallel : value mod contended portable
  type t

  (** [create ~max_domains ()] creates a scheduler that spawns worker threads on no more
      than [max_domains] domains. *)
  val create : ?max_domains:int (** default: [Multicore.max_domains ()] *) -> unit -> t

  (** [stop t] waits for pending tasks to complete and joins all worker domains.
      Attempting to schedule new tasks after calling [stop] will raise.

      During [stop], idle workers will not attempt to steal asynchronous tasks. *)
  val stop : t -> unit

  (** [is_stopped t] returns [true] if [t] has been stopped. *)
  val is_stopped : t -> bool

  (* $MDX part-begin=parallel *)

  (** [parallel t ~f] creates an implementation of parallelism backed by [t], applies [f],
      and waits for it to complete. *)
  val parallel : t -> f:(parallel @ local -> 'a) @ once shareable -> 'a

  (* $MDX part-end *)
end

module type S_concurrent = sig
  include S

  (** [concurrent t ~terminator ~f] creates an implementation of concurrency and
      parallelism backed by [t], applies [f], and waits for it to complete. Blocking
      operations in [f] may be terminated via [terminator].

      There is currently no mechanism to limit the creation rate of concurrent tasks. If
      concurrent work is generated faster than it can be executed, the scheduler's queues
      will grow unboundedly, leading to resource exhaustion.

      Running a concurrent task requires allocating a fiber, which consumes a significant
      chunk of memory and address space (similar to a thread). There is a lighter-weight
      implementation of fibers based on stack checks, but it is not currently available. *)
  val concurrent
    :  t
    -> terminator:Await.Terminator.t @ local
    -> f:(parallel Concurrent.t @ local portable -> 'a) @ once shareable
    -> 'a

  module Expert : sig
    (** [scheduler t] is a concurrent scheduler that runs tasks on [t]. *)
    val scheduler : t -> parallel Concurrent.Scheduler.t
  end
end
