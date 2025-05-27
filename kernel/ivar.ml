open! Base
open! Import

type 'a state =
  | Empty
  | Full of 'a @@ contended many portable

type ('a, 'k) inner : value mod contended portable =
  { mutex : 'k Capsule.Mutex.t
  ; cond : 'k Capsule.Condition.t
  ; flag : ('a state Unique.Ref.t, 'k) Capsule.Data.t
  }

type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

let create () =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let cond = Capsule.Condition.create () in
  let flag = Capsule.Data.create (fun () -> Unique.Ref.make Empty) in
  P { mutex; cond; flag }
;;

let fill_exn (P { mutex; cond; flag }) a =
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter flag ~password ~f:(fun flag ->
      match Unique.Ref.exchange flag (Full a) with
      | Full _ -> failwith "Ivar already filled."
      | Empty -> ());
    Capsule.Condition.broadcast cond)
  [@nontail]
;;

let wait (P { mutex; cond; flag }) =
  let rec aux key =
    let contents, key =
      Capsule.Key.access key ~f:(fun access ->
        Unique.Ref.exchange (Capsule.Data.unwrap ~access flag) Empty)
    in
    match contents with
    | Empty ->
      let key = Capsule.Condition.wait cond ~mutex key in
      aux key
    | Full a -> a, key
  in
  (Capsule.Mutex.with_key mutex ~f:(fun key ->
     let contents, key = aux key in
     { many = { portended = contents } }, key))
    .many
    .portended
;;

let ready (P { mutex; flag; _ }) =
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.extract flag ~password ~f:(fun flag ->
      match Unique.Ref.exchange flag Empty with
      | Empty -> false
      | Full _ as full ->
        Unique.Ref.set flag full;
        true))
  [@nontail]
;;
