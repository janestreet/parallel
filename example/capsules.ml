open! Base
module Capsule = Portable.Capsule.Expert

[@@@disable_unused_warnings]

let fork_join parallel =
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2
    parallel
    (fun _ -> (ref : _ @ uncontended))
    (fun _ -> (ref : _ @ uncontended))
;;

let increment ~(access : 'k Capsule.Access.t) ref =
  let ref = Capsule.Data.unwrap ~access ref in
  ref := !ref + 1
;;

let increment ~(password : 'k Capsule.Password.t) ref =
  Capsule.access ~password ~f:(fun access ->
    let ref = Capsule.Data.unwrap ~access ref in
    ref := !ref + 1)
;;

let () =
  let (Capsule.Key.P (key : _ Capsule.Key.t)) = Capsule.create () in
  ()
;;

let parallel_key parallel =
  let (P key) = Capsule.create () in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2
    parallel
    (fun _ ->
      Capsule.Key.with_password key ~f:(fun password -> increment ~password ref)
      |> (ignore : _ -> _))
    (fun _ -> ())
;;

let merge_fresh () =
  let (P key) = Capsule.create () in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  let access = Capsule.Key.destroy key in
  let ref = Capsule.Data.unwrap ~access ref in
  ref := !ref + 1
;;

let parallel_mutexes parallel =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2
    parallel
    (fun _ -> Capsule.Mutex.with_lock mutex ~f:(fun password -> increment ~password ref))
    (fun _ -> Capsule.Mutex.with_lock mutex ~f:(fun password -> increment ~password ref))
;;
