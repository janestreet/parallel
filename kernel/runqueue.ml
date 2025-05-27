open! Base
open! Import
module Job = Parallel_kernel1.Job
module Thunk = Parallel_kernel1.Thunk
include Parallel_kernel0.Runqueue

type 'a promoter = 'a Job.t @ once portable -> ('a Promise.t -> unit) @ local once

let[@loop always] rec promote_loop
  : nodes @ local -> n:int -> f:('a. 'a promoter) @ local -> nodes
  =
  fun (Q cursor) ~n ~f ->
  (Stack_pointer.use [@kind word]) cursor ~f:(function [@inline]
    | Some node when n > 0 -> promote_batch node ~n ~f
    | _ -> Q cursor)
    [@nontail]

and[@loop always] promote_batch
  : type l. l node @ local once -> n:int -> f:('a. 'a promoter) @ local -> nodes
  =
  fun node ~n ~f ->
  match node with
  | Cons1 t ->
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0;
    promote_loop t.down ~n:(n - 1) ~f
  | Cons2 t ->
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1;
    promote_loop t.down ~n:(n - 2) ~f
  | Cons3 t ->
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2;
    promote_loop t.down ~n:(n - 3) ~f
  | Cons4 t ->
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2;
    let p3 = Promise.start () in
    t.p3 <- This p3;
    f t.job3 p3;
    promote_loop t.down ~n:(n - 4) ~f
  | ConsN t ->
    (* Each batch of forked jobs must be promoted atomically. This enables us
       to check whether there has been a heartbeat in [with_jobs]. *)
    let p0 = Promise.start () in
    t.p0 <- This p0;
    f t.job0 p0;
    let p1 = Promise.start () in
    t.p1 <- This p1;
    f t.job1 p1;
    let p2 = Promise.start () in
    t.p2 <- This p2;
    f t.job2 p2;
    let p3 = Promise.start () in
    t.p3 <- This p3;
    f t.job3 p3;
    promote_batch t.more ~n:(n - 4) ~f
;;

let promote_n queue ~n ~(f : 'a. 'a promoter) =
  queue.cursor <- promote_loop queue.cursor ~n ~f
;;

let promote queue ~(f : 'a. 'a promoter) = promote_n queue ~n:Env.max_promotions ~f

let[@inline] [@loop always] rec node
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
  let[@inline] [@loop always] rec link : type l. l node @ local once -> unit = function
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
  let[@inline] [@loop always] rec unlink : type l. l node @ local once -> unit = function
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

let[@inline never] [@loop always] rec await
  : type a l.
    (a * l) node @ local once
    -> Parallel_kernel1.t @ local
    -> (a * l) Hlist.Gen(Panic.Result).t @ local portable unique
  =
  fun node parallel -> exclave_
  match node with
  | Cons1 { job0; p0; _ } ->
    let a = Promise.await (value_exn p0) job0 parallel in
    [ a ]
  | Cons2 { job0; p0; job1; p1; _ } ->
    let a = Promise.await (value_exn p0) job0 parallel in
    let b = Promise.await (value_exn p1) job1 parallel in
    [ a; b ]
  | Cons3 { job0; p0; job1; p1; job2; p2; _ } ->
    let a = Promise.await (value_exn p0) job0 parallel in
    let b = Promise.await (value_exn p1) job1 parallel in
    let c = Promise.await (value_exn p2) job2 parallel in
    [ a; b; c ]
  | Cons4 { job0; p0; job1; p1; job2; p2; job3; p3; _ } ->
    let a = Promise.await (value_exn p0) job0 parallel in
    let b = Promise.await (value_exn p1) job1 parallel in
    let c = Promise.await (value_exn p2) job2 parallel in
    let d = Promise.await (value_exn p3) job3 parallel in
    [ a; b; c; d ]
  | ConsN { more; job0; p0; job1; p1; job2; p2; job3; p3 } ->
    let a = Promise.await (value_exn p0) job0 parallel in
    let b = Promise.await (value_exn p1) job1 parallel in
    let c = Promise.await (value_exn p2) job2 parallel in
    let d = Promise.await (value_exn p3) job3 parallel in
    a :: b :: c :: d :: await more parallel
;;

let[@inline] [@loop always] rec apply
  : type l.
    l Hlist.Gen(Thunk).t @ contended once portable
    -> Parallel_kernel1.t @ local
    -> l Hlist.Gen(Panic.Result).t @ local portable unique
  =
  fun node parallel -> exclave_
  match node with
  | [] -> []
  | [ j0 ] ->
    let a = Thunk.apply j0 parallel in
    [ a ]
  | [ j0; j1 ] ->
    let a = Thunk.apply j0 parallel in
    let b = Thunk.apply j1 parallel in
    [ a; b ]
  | [ j0; j1; j2 ] ->
    let a = Thunk.apply j0 parallel in
    let b = Thunk.apply j1 parallel in
    let c = Thunk.apply j2 parallel in
    [ a; b; c ]
  | [ j0; j1; j2; j3 ] ->
    let a = Thunk.apply j0 parallel in
    let b = Thunk.apply j1 parallel in
    let c = Thunk.apply j2 parallel in
    let d = Thunk.apply j3 parallel in
    [ a; b; c; d ]
  | j0 :: j1 :: j2 :: j3 :: (_ :: _ as rest) ->
    let a = Thunk.apply j0 parallel in
    let b = Thunk.apply j1 parallel in
    let c = Thunk.apply j2 parallel in
    let d = Thunk.apply j3 parallel in
    a :: b :: c :: d :: apply rest parallel
;;

let[@inline] with_jobs
  :  t @ local -> 'a Thunk.t @ once portable
  -> ('b * 'l) Hlist.Gen(Thunk).t @ contended once portable -> Parallel_kernel1.t @ local
  -> 'a Panic.Result.t * ('b * 'l) Hlist.Gen(Panic.Result).t
     @ contended local portable unique
  =
  fun t first rest parallel -> exclave_
  (* Safe because [rest] is applied by either [apply] or [await], never both. The compiler
     is able to better optimize [apply rest] than a hypothetical [apply node]. *)
  let rest = (Obj.magic_many [@mode contended portable aliased]) rest in
  let node = node ~jobs:rest in
  Stack_pointer.unsafe_with_value node ~f:(fun [@inline] new_head ->
    exclave_
    let old_head = push_head t ~new_head in
    let first = Thunk.apply first parallel in
    pop_head t ~old_head;
    Stack_pointer.use new_head ~f:(function [@inline]
      | None -> assert false
      | Some node ->
        exclave_
        (match promoted node with
         | #(_, false) -> first, apply rest parallel
         | #(node, true) -> first, await node parallel)))
;;

module For_testing = struct
  let create () = exclave_
    { promote = (fun _ -> ())
    ; wake = (fun ~n:_ -> ())
    ; head = Q (Stack_pointer.null ())
    ; cursor = Q (Stack_pointer.null ())
    ; heartbeats = 0
    }
  ;;

  let promote t ~n ~f = promote_n t ~n ~f:(fun _ _ -> f ()) [@nontail]

  let with_jobs t jobs ~f =
    let node = node ~jobs in
    Stack_pointer.unsafe_with_value node ~f:(fun new_head : unit ->
      let old_head = push_head t ~new_head in
      f t;
      pop_head t ~old_head)
    [@nontail]
  ;;
end
