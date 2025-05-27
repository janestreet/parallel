open! Base

type t =
  | Domains
  | Grain
  | Length

let to_string = function
  | Domains -> "PARALLEL_BENCH_DOMAINS"
  | Grain -> "PARALLEL_BENCH_GRAIN"
  | Length -> "PARALLEL_BENCH_LENGTH"
;;

let get t ~default =
  match Sys.getenv (to_string t) with
  | None -> default
  | Some i -> Int.of_string i
;;

let domains = get Domains ~default:4
let grain = get Grain ~default:16
let length = get Length ~default:1_000_000
