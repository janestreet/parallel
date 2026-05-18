open! Base
open! Import

module rec Job : sig @@ portable
  type 'a t = Parallel.t @ local -> 'a Result.Capsule.t @ local unique
end =
  Job

and Thunk : sig @@ portable
  type 'a t = Parallel.t @ local -> 'a
end =
  Thunk

and Ops : sig @@ portable
  (* Subtasks must not [Await] or [Yield]; [Parallel.t] does not provide the capability. *)
  type 'a t =
    | Promise : 'a Promise.t -> ('a Result.Capsule.t * tokens:int) t
    (** When filled, pushes the continuation to the local queue. Used by [Promise.t]. *)
    | Await : Trigger.t -> unit t
    (** When signaled, pushes the continuation to the global queue. Used by [Await.t]. *)
    | Yield : unit t
    (** Immediately pushes the continuation to the global queue. Used by [Yield.t]. *)
end =
  Ops

and Wait : (Handled_effect.S with type ('a, _) ops := 'a Ops.t) = Handled_effect.Make (Ops)

and Promise : sig @@ portable
  type 'a continuation =
    ('a, (unit, unit) Wait.Contended.Result.t, unit) Handled_effect.Continuation.t

  type 'a state =
    | Start
    | Claimed
    | Blocking :
        { key : 'k Capsule.Key.t @@ many
        ; cont : (('a Result.Capsule.t * tokens:int) continuation, 'k) Capsule.Data.t
          @@ many
        }
        -> 'a state
    | Ready of
        { result : 'a Result.Capsule.t @@ contended many portable
        ; tokens : int
        }

  type 'a t = 'a state Unique.Atomic.t
end =
  Promise

and Runqueue : sig @@ portable
  type _ node =
    | Cons1 :
        { mutable promise : 'a Promise.t or_null
        ; job : 'a Job.t @@ global portable
        ; mutable down : nodes
        }
        -> ('a * unit) node
    | ConsN :
        { mutable promise : 'a Promise.t or_null
        ; job : 'a Job.t @@ global portable
        ; more : ('b * 'l) node
        }
        -> ('a * ('b * 'l)) node

  and nodes = Q : _ node Stack_pointer.t -> nodes [@@unboxed]

  type t =
    { mutable tokens : int
    ; mutable head : nodes
    ; mutable cursor : nodes
    }
end =
  Runqueue

and Scheduler : sig @@ portable
  type t : (value & value & value) mod contended portable =
    #{ task : (unit -> unit) @ once portable -> unit @@ portable
     (** Pushes a concurrent task (which may await/yield) to the global queue. *)
     ; subtask : (unit -> unit) @ once portable -> unit @@ portable
     (** Pushes a parallel subtask (which can only sync) to the local queue. *)
     ; try_wake : n:int -> unit @@ portable
     (** Signals up to [n] workers that are currently asleep. *)
     }
end =
  Scheduler

and Parallel : sig @@ portable
  type t =
    | Sequential
    | Parallel :
        { password : 'k Capsule.Password.t @@ many
        ; queue : (Runqueue.t, 'k) Capsule.Data.t
        ; handler : Wait.t Handled_effect.Handler.t @@ contended portable
        ; scheduler : Scheduler.t @@ global many
        }
        -> t
end =
  Parallel
