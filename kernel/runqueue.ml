open! Base
open! Import
module Job = Parallel_kernel1.Job
module Thunk = Parallel_kernel1.Thunk
include Parallel_kernel0.Runqueue

type 'a promoter =
  'a Job.t @ once portable
  -> ('a Promise.t -> (tokens:int -> unit) @ local once) @ local once

let[@loop] rec promote_loop
  : nodes @ local -> tok:int -> f:('a. 'a promoter) @ local -> #(nodes * int)
  =
  fun (Q cursor) ~tok ~f ->
  (Stack_pointer.use [@kind word & value]) cursor ~f:(function [@inline]
    | Some node when tok > 0 -> promote_batch node ~tok ~f
    | _ -> #(Q cursor, tok))
    [@nontail]

and[@loop] promote_batch
  : type l.
    l node @ local once -> tok:int -> f:('a. 'a promoter) @ local -> #(nodes * int)
  =
  fun node ~tok ~f ->
  (* Each batch of forked jobs must be promoted atomically. This enables us to
     check whether there has been a heartbeat in [with_jobs].

     Given [n] jobs, we give each one [(tok - n) / (n + 1)] tokens. This spends
     one token per promotion, then evenly divides the remaining tokens between
     the children, including the current fiber (which retains the remainder).
     Note it is possible to return a negative count. *)
  match node with
  | Cons1 t ->
    let tok = tok - 1 in
    let tokens = tok / 2 in
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0 ~tokens;
    promote_loop t.down ~tok:(tok - tokens) ~f
  | Cons2 t ->
    let tok = tok - 2 in
    let tokens = tok / 3 in
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0 ~tokens;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1 ~tokens;
    promote_loop t.down ~tok:(tok - (2 * tokens)) ~f
  | Cons3 t ->
    let tok = tok - 3 in
    let tokens = tok / 4 in
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0 ~tokens;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1 ~tokens;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2 ~tokens;
    promote_loop t.down ~tok:(tok - (3 * tokens)) ~f
  | Cons4 t ->
    let tok = tok - 4 in
    let tokens = tok / 5 in
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0 ~tokens;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1 ~tokens;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2 ~tokens;
    let p3 = Promise.start () in
    t.p3 <- This p3;
    f t.job3 p3 ~tokens;
    promote_loop t.down ~tok:(tok - (4 * tokens)) ~f
  | ConsN t ->
    let tok = tok - 4 in
    let tokens = tok / 5 in
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0 ~tokens;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1 ~tokens;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2 ~tokens;
    let p3 = Promise.start () in
    t.p3 <- This p3;
    f t.job3 p3 ~tokens;
    promote_batch t.more ~tok:(tok - (4 * tokens)) ~f
;;

let do_promote queue ~(f : 'a. 'a promoter) =
  let #(cursor, remaining) = promote_loop queue.cursor ~tok:queue.tokens ~f in
  let promoted = queue.tokens - remaining in
  queue.tokens <- remaining;
  queue.cursor <- cursor;
  promoted
;;

let promote queue ~add_tokens =
  (* If we encounter an async heartbeat during promotion (either from
     [use_tokens] or a double heartbeat), we skip it. Note promotions happen
     concurrently but *not* in parallel. *)
  let scheduler = queue.scheduler in
  if not queue.promoting
  then (
    queue.promoting <- true;
    queue.tokens <- queue.tokens + add_tokens;
    let promoted =
      do_promote queue ~f:(fun job promise ~tokens ->
        scheduler.#promote (Promise.fiber promise job ~scheduler ~tokens))
    in
    if promoted > 0 then scheduler.#wake ~n:promoted;
    queue.promoting <- false)
;;

let[@inline] [@loop] rec node
  : type a l.
    jobs:(a * l) Hlist.Gen(Thunk).t @ contended once portable -> (a * l) node @ local once
  =
  fun ~jobs -> exclave_
  let down = Q (Stack_pointer.null ()) in
  match jobs with
  | [ job0 ] -> Cons1 { job0 = Job.wrap job0; p0 = Null; down }
  | [ job0; job1 ] ->
    Cons2 { job0 = Job.wrap job0; p0 = Null; job1 = Job.wrap job1; p1 = Null; down }
  | [ job0; job1; job2 ] ->
    Cons3
      { job0 = Job.wrap job0
      ; p0 = Null
      ; job1 = Job.wrap job1
      ; p1 = Null
      ; job2 = Job.wrap job2
      ; p2 = Null
      ; down
      }
  | [ job0; job1; job2; job3 ] ->
    Cons4
      { job0 = Job.wrap job0
      ; p0 = Null
      ; job1 = Job.wrap job1
      ; p1 = Null
      ; job2 = Job.wrap job2
      ; p2 = Null
      ; job3 = Job.wrap job3
      ; p3 = Null
      ; down
      }
  | job0 :: job1 :: job2 :: job3 :: (_ :: _ as jobs) ->
    ConsN
      { job0 = Job.wrap job0
      ; p0 = Null
      ; job1 = Job.wrap job1
      ; p1 = Null
      ; job2 = Job.wrap job2
      ; p2 = Null
      ; job3 = Job.wrap job3
      ; p3 = Null
      ; more = node ~jobs
      }
;;

let[@inline] push_head t ~new_head =
  let new_head = Q new_head in
  (* Inlining won't tell us [l], so we would like the Cons1-4 cases to compile
     to the same store instruction. However, [down] has to be the last field,
     so it does not have the same offset in each inline record. *)
  let[@inline] [@loop] rec link : type l. l node @ local once -> unit = function
    | Cons1 node -> node.down <- new_head
    | Cons2 node -> node.down <- new_head
    | Cons3 node -> node.down <- new_head
    | Cons4 node -> node.down <- new_head
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
  (* Inlining won't tell us [l], so we would like the Cons1-4 cases to compile
     to the same store instruction. However, [down] has to be the last field,
     so it does not have the same offset in each inline record. *)
  let[@inline] [@loop] rec unlink : type l. l node @ local once -> unit = function
    | Cons1 node -> node.down <- null
    | Cons2 node -> node.down <- null
    | Cons3 node -> node.down <- null
    | Cons4 node -> node.down <- null
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

let[@inline] promoted : type l. l node @ local once -> #(l node * bool) @ local once =
  fun node ->
  match node with
  | Cons1 { p0 = This _; _ }
  | Cons2 { p0 = This _; _ }
  | Cons3 { p0 = This _; _ }
  | Cons4 { p0 = This _; _ }
  | ConsN { p0 = This _; _ } -> #(node, true)
  | _ -> #(node, false)
;;

let[@inline] value_exn = function
  | This a -> a
  | Null -> failwith "Null"
;;

let[@inline never] [@loop] rec await
  : type a l.
    (a * l) node @ local once
    -> Parallel_kernel1.t @ local
    -> (a * l) Hlist.Gen(Result.Capsule).t @ local portable unique unyielding
  =
  fun node parallel -> exclave_
  match node with
  | Cons1 { job0; p0; _ } ->
    let a = Promise.await_or_run (value_exn p0) job0 parallel in
    [ a ]
  | Cons2 { job0; p0; job1; p1; _ } ->
    let a = Promise.await_or_run (value_exn p0) job0 parallel in
    let b = Promise.await_or_run (value_exn p1) job1 parallel in
    [ a; b ]
  | Cons3 { job0; p0; job1; p1; job2; p2; _ } ->
    let a = Promise.await_or_run (value_exn p0) job0 parallel in
    let b = Promise.await_or_run (value_exn p1) job1 parallel in
    let c = Promise.await_or_run (value_exn p2) job2 parallel in
    [ a; b; c ]
  | Cons4 { job0; p0; job1; p1; job2; p2; job3; p3; _ } ->
    let a = Promise.await_or_run (value_exn p0) job0 parallel in
    let b = Promise.await_or_run (value_exn p1) job1 parallel in
    let c = Promise.await_or_run (value_exn p2) job2 parallel in
    let d = Promise.await_or_run (value_exn p3) job3 parallel in
    [ a; b; c; d ]
  | ConsN { more; job0; p0; job1; p1; job2; p2; job3; p3 } ->
    let a = Promise.await_or_run (value_exn p0) job0 parallel in
    let b = Promise.await_or_run (value_exn p1) job1 parallel in
    let c = Promise.await_or_run (value_exn p2) job2 parallel in
    let d = Promise.await_or_run (value_exn p3) job3 parallel in
    a :: b :: c :: d :: await more parallel
;;

let[@inline] [@loop] rec apply
  : type l.
    l Hlist.Gen(Thunk).t @ contended once portable
    -> Parallel_kernel1.t @ local
    -> l Hlist.Gen(Result.Capsule).t @ local portable unique unyielding
  =
  fun node parallel -> exclave_
  match node with
  | [] -> []
  | [ j0 ] ->
    let a = Thunk.encapsulate j0 parallel in
    [ a ]
  | [ j0; j1 ] ->
    let a = Thunk.encapsulate j0 parallel in
    let b = Thunk.encapsulate j1 parallel in
    [ a; b ]
  | [ j0; j1; j2 ] ->
    let a = Thunk.encapsulate j0 parallel in
    let b = Thunk.encapsulate j1 parallel in
    let c = Thunk.encapsulate j2 parallel in
    [ a; b; c ]
  | [ j0; j1; j2; j3 ] ->
    let a = Thunk.encapsulate j0 parallel in
    let b = Thunk.encapsulate j1 parallel in
    let c = Thunk.encapsulate j2 parallel in
    let d = Thunk.encapsulate j3 parallel in
    [ a; b; c; d ]
  | j0 :: j1 :: j2 :: j3 :: (_ :: _ as rest) ->
    let a = Thunk.encapsulate j0 parallel in
    let b = Thunk.encapsulate j1 parallel in
    let c = Thunk.encapsulate j2 parallel in
    let d = Thunk.encapsulate j3 parallel in
    a :: b :: c :: d :: apply rest parallel
;;

let[@inline] with_jobs
  : ('a : value mod portable) 'b.
  t @ local
  -> 'a Thunk.t @ local once
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable
  -> Parallel_kernel1.t @ local
  -> 'a Result.t * ('b * 'l) Hlist.Gen(Result.Capsule).t
     @ local portable unique unyielding
  =
  fun t first rest parallel -> exclave_
  (* Safe because [rest] is applied by either [apply] or [await], never both. The compiler
     is able to better optimize [apply rest] than a hypothetical [apply node]. *)
  let rest = (Obj.magic_many [@mode contended portable]) rest in
  let node = node ~jobs:rest in
  (Stack_pointer.unsafe_with_value node ~f:(fun [@inline] new_head -> exclave_
     let old_head = push_head t ~new_head in
     let first = Thunk.apply first parallel in
     pop_head t ~old_head;
     Stack_pointer.use new_head ~f:(function [@inline]
       | None -> assert false
       | Some node ->
         exclave_
         (match promoted node with
          | #(_, false) -> { unyielding = first, apply rest parallel }
          | #(node, true) -> { unyielding = first, await node parallel }))
       [@nontail]))
    .unyielding
;;

module For_testing = struct
  let create () = exclave_
    { tokens = 0
    ; promoting = false
    ; head = Q (Stack_pointer.null ())
    ; cursor = Q (Stack_pointer.null ())
    ; scheduler = #{ promote = (fun _ -> ()); wake = (fun ~n:_ -> ()) }
    }
  ;;

  let promote t ~n ~f =
    t.tokens <- n;
    do_promote t ~f:(fun _ _ ~tokens:_ -> f ()) |> (ignore : int -> unit)
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
