open! Core
open Parallel_command_intf
include Definitions
open Async.Command

let parallel ?max_workers ~summary ?readme param =
  basic
    ~summary
    ?readme
    (Param.map param ~f:(fun main () ->
       Parallel_scheduler.with_parallel ?max_workers main))
;;

let parallel_or_error ?max_workers ~summary ?readme param =
  basic_or_error
    ~summary
    ?readme
    (Param.map param ~f:(fun main () ->
       Parallel_scheduler.with_parallel ?max_workers main))
;;

let concurrent ?max_workers ~summary ?readme param =
  basic
    ~summary
    ?readme
    (Param.map param ~f:(fun main () ->
       Parallel_scheduler.with_concurrent ?max_workers main))
;;

let concurrent_or_error ?max_workers ~summary ?readme param =
  basic_or_error
    ~summary
    ?readme
    (Param.map param ~f:(fun main () ->
       Parallel_scheduler.with_concurrent ?max_workers main))
;;

let with_scheduler ?max_workers main =
  let sched = Parallel_scheduler.scheduler ?max_workers () in
  main sched
;;

let async ?behave_nicely_in_pipeline ?extract_exn ?max_workers ~summary ?readme param =
  async
    ~summary
    ?readme
    ?behave_nicely_in_pipeline
    ?extract_exn
    (Param.map param ~f:(fun main () -> with_scheduler ?max_workers main))
;;

let async_or_error
  ?behave_nicely_in_pipeline
  ?extract_exn
  ?max_workers
  ~summary
  ?readme
  param
  =
  async_or_error
    ~summary
    ?readme
    ?behave_nicely_in_pipeline
    ?extract_exn
    (Param.map param ~f:(fun main () -> with_scheduler ?max_workers main))
;;

module Staged = struct
  include Async.Command.Staged

  let async ?behave_nicely_in_pipeline ?extract_exn ?max_workers ~summary ?readme param =
    Staged.async
      ~summary
      ?readme
      ?behave_nicely_in_pipeline
      ?extract_exn
      (Param.map param ~f:(fun main () -> with_scheduler ?max_workers main))
  ;;

  let async_or_error
    ?behave_nicely_in_pipeline
    ?extract_exn
    ?max_workers
    ~summary
    ?readme
    param
    =
    Staged.async_or_error
      ~summary
      ?readme
      ?behave_nicely_in_pipeline
      ?extract_exn
      (Param.map param ~f:(fun main () -> with_scheduler ?max_workers main))
  ;;
end
