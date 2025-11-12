open! Base

type t =
  | Max_domains
  | Length
  | Eager

let to_string = function
  | Max_domains -> "PARALLEL_BENCH_MAX_DOMAINS"
  | Length -> "PARALLEL_BENCH_LENGTH"
  | Eager -> "PARALLEL_BENCH_EAGER"
;;

let get t ~default =
  match Sys.getenv (to_string t) with
  | None -> default
  | Some i -> Int.of_string i
;;

let max_domains = get Max_domains ~default:4096
let length = get Length ~default:1_000_000
let eager = get Eager ~default:0
