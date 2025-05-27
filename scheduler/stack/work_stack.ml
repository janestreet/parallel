open! Base
module Capsule = Portable.Capsule.Expert

module Once_stack : sig @@ portable
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a @ once -> unit
  val pop : 'a t -> 'a option @ once
end = struct
  type 'a t = 'a Stack.t

  let create = Stack.create
  let pop = Stack.pop

  (* Safety: elements are pushed into the stack exactly once, and popped exactly once. *)
  let[@inline] push t a = Stack.push t (Obj.magic_many a)
end

type 'k inner =
  { mutex : 'k Capsule.Mutex.t
  ; cond : 'k Capsule.Condition.t
  ; stack : ((unit -> unit) portable Once_stack.t, 'k) Capsule.Data.t
  }

type t = P : 'k inner -> t [@@unboxed]

let create () =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let cond = Capsule.Condition.create () in
  let stack = Capsule.Data.create (fun () -> Once_stack.create ()) in
  P { mutex; cond; stack }
;;

let push (P { mutex; cond; stack; _ }) ~(f : unit -> unit @@ once portable) =
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter stack ~password ~f:(fun stack ->
      Once_stack.push stack { portable = f });
    Capsule.Condition.signal cond)
  [@nontail]
;;

let wake (P { mutex; cond; _ }) =
  (* Must be atomic with respect to [break] and going to sleep in [work]. *)
  Capsule.Mutex.with_lock mutex ~f:(fun _ -> Capsule.Condition.broadcast cond) [@nontail]
;;

let work (P { mutex; cond; stack }) ~break =
  let rec next key =
    let { aliased = contents }, key =
      Capsule.Key.access key ~f:(fun access ->
        { aliased = Once_stack.pop (Capsule.Data.unwrap ~access stack) })
    in
    match contents with
    | None ->
      (match break () with
       | true -> { aliased = None }, key
       | false ->
         let key = Capsule.Condition.wait cond ~mutex key in
         next key)
    | Some _ as task -> { aliased = task }, key
  in
  let rec go () =
    match (Capsule.Mutex.with_key mutex ~f:next).aliased with
    | None -> ()
    | Some { portable = task } ->
      task ();
      go ()
  in
  go () [@nontail]
;;
