open! Base
open! Import

type t =
  | Heartbeat_promotions
  | Heartbeat_interval_us

let to_string = function
  | Heartbeat_promotions -> "PARALLEL_HEARTBEAT_PROMOTIONS"
  | Heartbeat_interval_us -> "PARALLEL_HEARTBEAT_INTERVAL_US"
;;

let get ?(min = Int.min_value) ?(max = Int.max_value) t ~default =
  let i =
    match Sys.getenv (to_string t) with
    | None -> default
    | Some i -> Int.of_string i
  in
  if i < min then Printf.failwithf !"%{} < %d" t min ();
  if i > max then Printf.failwithf !"%{} > %d" t max ();
  i
;;

let heartbeat_promotions = get Heartbeat_promotions ~default:10 ~min:0
let heartbeat_interval_us = get Heartbeat_interval_us ~default:100 ~min:1 ~max:999_999
