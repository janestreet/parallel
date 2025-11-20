open! Base
open! Import
module Job = Parallel_kernel1.Job
module Thunk = Parallel_kernel1.Thunk
include Parallel_kernel0.Runqueue

type 'a promoter =
  'a Job.t @ once portable
  -> ('a Promise.t -> (tokens:int -> unit) @ local once) @ local once

let[@loop] rec count : type l. l node @ local once -> int = function
  | Cons1 _ -> 1
  | ConsN { more; _ } -> 1 + count more
;;

let[@loop] rec promote_loop
  :  nodes @ local -> tokens:int -> promoted:int -> f:('a. 'a promoter) @ local
  -> #(nodes * tokens:int * promoted:int)
  =
  fun (Q cursor) ~tokens ~promoted ~f ->
  (* Given [n] jobs, we assign each one [(tok - n) / (n + 1)] tokens. This spends one
     token per promotion, then evenly divides the remaining tokens between the children,
     including the current fiber (which retains the remainder). Note it is possible to
     return a negative count. *)
  (Stack_pointer.use [@kind word & value & value]) cursor ~f:(function [@inline]
    | Some node when tokens > 0 ->
      exclave_
      (* Safe since [count] does not consume [node]. *)
      let node = Obj.magic_many node in
      let count = count node in
      let tokens = tokens - count in
      promote_batch node ~tokens ~promoted ~each:(tokens / (count + 1)) ~f
    | _ -> #(Q cursor, ~tokens, ~promoted))
    [@nontail]

and[@loop] promote_batch
  : type l.
    l node @ local once
    -> tokens:int
    -> promoted:int
    -> each:int
    -> f:('a. 'a promoter) @ local
    -> #(nodes * tokens:int * promoted:int)
  =
  fun node ~tokens ~promoted ~each ~f ->
  (* Each batch of forked jobs is promoted atomically. *)
  match node with
  | Cons1 t ->
    let promise = Promise.start () in
    t.promise <- This promise;
    f t.job promise ~tokens:each;
    promote_loop t.down ~tokens:(tokens - each) ~promoted:(promoted + 1) ~f
  | ConsN t ->
    let promise = Promise.start () in
    t.promise <- This promise;
    f t.job promise ~tokens:each;
    promote_batch t.more ~tokens:(tokens - each) ~promoted:(promoted + 1) ~each ~f
;;

(* Inline never to guarantee no poll points between reading and writing [queue.tokens].
   [queue.tokens] could be atomic, but we don't want any extra cost given it is only
   accessed by one thread. *)
let[@inline never] add_tokens queue tokens = queue.tokens <- queue.tokens + tokens

let promotions queue ~(f : 'a. 'a promoter) =
  let start = queue.tokens in
  let #(cursor, ~tokens:stop, ~promoted) =
    promote_loop queue.cursor ~tokens:start ~promoted:0 ~f
  in
  (* [tokens] may be incremented concurrently, so we do not set it directly to [stop]. *)
  let used = start - stop in
  add_tokens queue ~-used;
  queue.cursor <- cursor;
  promoted
;;

let promote queue ~(scheduler : Parallel_kernel0.Scheduler.t) =
  let promoted =
    promotions queue ~f:(fun job promise ~tokens ->
      scheduler.#promote (Promise.try_fiber promise job ~scheduler ~tokens))
  in
  if promoted > 0 then scheduler.#wake ~n:promoted
;;

let[@inline] [@loop] rec node
  : type a l.
    jobs:(a * l) Hlist.Gen(Thunk).t @ contended once portable -> (a * l) node @ local once
  =
  fun ~jobs -> exclave_
  match jobs with
  | [ job ] ->
    Cons1 { job = Job.wrap job; promise = Null; down = Q (Stack_pointer.null ()) }
  | job :: (_ :: _ as jobs) ->
    ConsN { job = Job.wrap job; promise = Null; more = node ~jobs }
;;

let[@inline] push_head t ~new_head =
  let new_head = Q new_head in
  let[@inline] [@loop] rec link : type l. l node @ local once -> unit = function
    | Cons1 node -> node.down <- new_head
    | ConsN node -> link node.more
  in
  let (Q head as old_head) = t.head in
  Stack_pointer.use head ~f:(function [@inline]
    | None -> ()
    | Some head -> link head);
  t.head <- new_head;
  let (Q cursor) = t.cursor in
  Stack_pointer.use cursor ~f:(function [@inline]
    | None -> t.cursor <- new_head
    | Some _ -> ());
  old_head
;;

let[@inline] pop_head t ~old_head =
  let null = Q (Stack_pointer.null ()) in
  let[@inline] [@loop] rec unlink : type l. l node @ local once -> unit = function
    | Cons1 node -> node.down <- null
    | ConsN node -> unlink node.more
  in
  let (Q old_head) = old_head in
  Stack_pointer.use old_head ~f:(function [@inline]
    | None -> ()
    | Some old_head -> unlink old_head);
  let (Q new_head) = t.head in
  let (Q new_cursor) = t.cursor in
  if Stack_pointer.equal new_cursor new_head then t.cursor <- null;
  t.head <- Q old_head
;;

let[@inline] value_exn = function
  | This a -> a
  | Null -> failwith "Null"
;;

let[@inline never] [@loop] rec await
  : type a l.
    (a * l) node @ local once
    -> Parallel_kernel1.t @ local
    -> (a * l) Hlist.Gen(Result.Capsule).t @ forkable local portable unique
  =
  fun node parallel -> exclave_
  match node with
  | Cons1 { job; promise; _ } ->
    let a = Promise.await_or_run (value_exn promise) job parallel in
    [ a ]
  | ConsN { job; promise; more } ->
    let a = Promise.await_or_run (value_exn promise) job parallel in
    a :: await more parallel
;;

let[@inline] [@loop] rec apply
  : type a l.
    (a * l) Hlist.Gen(Thunk).t @ contended once portable
    -> Parallel_kernel1.t @ local
    -> (a * l) Hlist.Gen(Result.Capsule).t @ forkable local portable unique
  =
  fun node parallel -> exclave_
  match node with
  | [ job ] ->
    let a = Thunk.encapsulate job parallel in
    [ a ]
  | job :: (_ :: _ as rest) ->
    let a = Thunk.encapsulate job parallel in
    a :: apply rest parallel
;;

let[@inline] [@loop] rec with_node
  : type (a : value mod portable) b l.
    t @ local
    -> (Parallel_kernel1.t @ local -> a @ forkable local unique) @ local once
    -> (b * l) Hlist.Gen(Thunk).t @ contended once portable
    -> (b * l) node @ local once
    -> Parallel_kernel1.t @ local
    -> a * (b * l) Hlist.Gen(Result.Capsule).t @ forkable local portable unique
  =
  fun t first rest node parallel -> exclave_
  (Stack_pointer.unsafe_with_value node ~f:(fun [@inline] new_head -> exclave_
     let old_head = push_head t ~new_head in
     let a = first parallel in
     pop_head t ~old_head;
     Stack_pointer.use new_head ~f:(function [@inline]
       | None -> assert false
       | Some node ->
         exclave_
         (match node with
          | Cons1 { promise = This _; _ } | ConsN { promise = This _; _ } ->
            { forkable = a, await node parallel }
          | Cons1 { promise = Null; _ } -> { forkable = a, apply rest parallel }
          | ConsN { promise = Null; more; _ } ->
            let (first :: rest) = rest in
            let[@inline] first parallel = exclave_ Thunk.encapsulate first parallel in
            let b, rest = with_node t first rest more parallel in
            { forkable = a, b :: rest }))
       [@nontail]))
    .forkable
;;

let[@inline] with_jobs
  : ('a : value mod portable) 'b.
  t @ local
  -> 'a Thunk.t @ local once
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable
  -> Parallel_kernel1.t @ local
  -> 'a Result.t * ('b * 'l) Hlist.Gen(Result.Capsule).t @ forkable local portable unique
  =
  fun t first rest parallel -> exclave_
  let[@inline] first parallel = exclave_ Thunk.apply first parallel in
  (* Safe because [rest] is applied by either [apply] or [await], never both. The compiler
     is able to better optimize [apply rest] than a hypothetical [apply node]. *)
  let rest = (Obj.magic_many [@mode contended portable]) rest in
  let node = node ~jobs:rest in
  with_node t first rest node parallel [@nontail]
;;

module For_testing = struct
  let create () = exclave_
    { tokens = 0; head = Q (Stack_pointer.null ()); cursor = Q (Stack_pointer.null ()) }
  ;;

  let tokens t = t.tokens

  let promote t ~n ~f =
    t.tokens <- n;
    promotions t ~f:(fun _ _ ~tokens -> f ~tokens) |> (ignore : int -> unit)
  ;;

  let with_jobs t jobs ~f =
    let node = node ~jobs in
    Stack_pointer.unsafe_with_value node ~f:(fun new_head : unit ->
      let old_head = push_head t ~new_head in
      f t;
      pop_head t ~old_head)
    [@nontail]
  ;;
end
