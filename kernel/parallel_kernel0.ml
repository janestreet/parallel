open! Base
open! Import

module rec Job : sig @@ portable
  type 'a t = Parallel.t @ local -> 'a Panic.Result.t @ local unique
end =
  Job

and Thunk : sig @@ portable
  type 'a t = Parallel.t @ local -> 'a
end =
  Thunk

and Ops : sig @@ portable
  type 'a t = Await : 'a Promise.t -> 'a Panic.Result.t t [@@unboxed]
end =
  Ops

and Await : (Effect.S with type ('a, _) ops := 'a Ops.t) = Effect.Make (Ops)

and Promise : sig @@ portable
  type 'a continuation =
    ( 'a Panic.Result.t portable
      , (unit, unit) Await.Contended.Result.t
      , unit )
      Effect.Continuation.t

  type 'a state =
    | Start
    | Claimed
    | Blocking :
        { key : 'k Capsule.Key.t @@ many
        ; cont : ('a continuation, 'k) Capsule.Data.t @@ many
        }
        -> 'a state
    | Ready of 'a Panic.Result.t @@ contended many portable

  type 'a t = 'a state Unique.Atomic.t
end =
  Promise

and Runqueue : sig @@ portable
  type _ node : value mod portable =
    | Cons1 :
        { mutable p0 : 'a Promise.t or_null
        ; job0 : 'a Job.t @@ global portable
        ; mutable down : nodes
        }
        -> ('a * unit) node
    | Cons2 :
        { mutable p0 : 'a Promise.t or_null
        ; mutable p1 : 'b Promise.t or_null
        ; job0 : 'a Job.t @@ global portable
        ; job1 : 'b Job.t @@ global portable
        ; mutable down : nodes
        }
        -> ('a * ('b * unit)) node
    | Cons3 :
        { mutable p0 : 'a Promise.t or_null
        ; mutable p1 : 'b Promise.t or_null
        ; mutable p2 : 'c Promise.t or_null
        ; job0 : 'a Job.t @@ global portable
        ; job1 : 'b Job.t @@ global portable
        ; job2 : 'c Job.t @@ global portable
        ; mutable down : nodes
        }
        -> ('a * ('b * ('c * unit))) node
    | Cons4 :
        { mutable p0 : 'a Promise.t or_null
        ; mutable p1 : 'b Promise.t or_null
        ; mutable p2 : 'c Promise.t or_null
        ; mutable p3 : 'd Promise.t or_null
        ; job0 : 'a Job.t @@ global portable
        ; job1 : 'b Job.t @@ global portable
        ; job2 : 'c Job.t @@ global portable
        ; job3 : 'd Job.t @@ global portable
        ; mutable down : nodes
        }
        -> ('a * ('b * ('c * ('d * unit)))) node
    | ConsN :
        { mutable p0 : 'a Promise.t or_null
        ; mutable p1 : 'b Promise.t or_null
        ; mutable p2 : 'c Promise.t or_null
        ; mutable p3 : 'd Promise.t or_null
        ; job0 : 'a Job.t @@ global portable
        ; job1 : 'b Job.t @@ global portable
        ; job2 : 'c Job.t @@ global portable
        ; job3 : 'd Job.t @@ global portable
        ; more : ('e * 'l) node
        }
        -> ('a * ('b * ('c * ('d * ('e * 'l))))) node
  [@@unsafe_allow_any_mode_crossing]

  and nodes = Q : _ node Stack_pointer.t -> nodes [@@unboxed]

  type t =
    { promote : (unit -> unit) @ once portable -> unit @@ global portable
    ; wake : n:int -> unit @@ global portable
    ; mutable heartbeats : int
    ; mutable head : nodes
    ; mutable cursor : nodes
    }
end =
  Runqueue

and Scheduler : sig @@ portable
  type t =
    #{ monitor : Panic.Monitor.t
     ; promote : (unit -> unit) @ once portable -> unit @@ portable
     ; wake : n:int -> unit @@ portable
     }
end =
  Scheduler

and Parallel : sig @@ portable
  type t : value mod contended portable =
    | Sequential of Panic.Monitor.t @@ global
    | Parallel :
        { monitor : Panic.Monitor.t @@ global
        ; password : 'k Capsule.Password.t
        ; queue : (Runqueue.t, 'k) Capsule.Data.t
        ; handler : Await.t Effect.Handler.t @@ contended portable
        }
        -> t
  [@@unsafe_allow_any_mode_crossing]
end =
  Parallel
