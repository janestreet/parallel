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
  type 'a t =
    | Promise : 'a Promise.t -> 'a Result.Capsule.t t
    | Trigger : Await.Trigger.t -> unit t
end =
  Ops

and Wait : (Effect.S with type ('a, _) ops := 'a Ops.t) = Effect.Make (Ops)

and Promise : sig @@ portable
  type 'a continuation =
    ('a portable, (unit, unit) Wait.Contended.Result.t, unit) Effect.Continuation.t

  type 'a state =
    | Start
    | Claimed
    | Blocking :
        { key : 'k Capsule.Key.t @@ many
        ; cont : ('a Result.Capsule.t continuation, 'k) Capsule.Data.t @@ many
        }
        -> 'a state
    | Ready of 'a Result.Capsule.t @@ contended many portable

  type 'a t = 'a state Unique.Atomic.t
end =
  Promise

and Runqueue : sig @@ portable
  type _ node : value mod portable =
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
  [@@unsafe_allow_any_mode_crossing]

  and nodes = Q : _ node Stack_pointer.t -> nodes [@@unboxed]

  type t =
    { mutable tokens : int
    ; mutable head : nodes
    ; mutable cursor : nodes
    }
end =
  Runqueue

and Scheduler : sig @@ portable
  type t : (value & value) mod contended portable =
    #{ promote : (unit -> unit) @ once portable -> unit @@ portable
     ; wake : n:int -> unit @@ portable
     }
end =
  Scheduler

and Parallel : sig @@ portable
  type%fuelproof t : value mod contended portable =
    | Sequential
    | Parallel :
        { password : 'k Capsule.Password.t @@ many
        ; queue : (Runqueue.t, 'k) Capsule.Data.t
        ; handler : Wait.t Effect.Handler.t @@ contended portable
        ; scheduler : Scheduler.t @@ global many
        }
        -> t
end =
  Parallel
