open! Core
open Parallel_command_intf
include Definitions
open Async.Command

let parallel ?max_domains ~summary ?readme param =
  basic
    ~summary
    ?readme
    (Param.map param ~f:(fun main () -> main (Parallel_scheduler.create ?max_domains ())))
;;

let parallel_or_error ?max_domains ~summary ?readme param =
  basic_or_error
    ~summary
    ?readme
    (Param.map param ~f:(fun main () -> main (Parallel_scheduler.create ?max_domains ())))
;;

let parallel_async
  ?behave_nicely_in_pipeline
  ?extract_exn
  ?max_domains
  ~summary
  ?readme
  param
  =
  async
    ~summary
    ?readme
    ?behave_nicely_in_pipeline
    ?extract_exn
    (Param.map param ~f:(fun main () ->
       main
         (Parallel_async.create
            (module Parallel_scheduler)
            (Parallel_scheduler.create ?max_domains ()))))
;;

let parallel_async_or_error
  ?behave_nicely_in_pipeline
  ?extract_exn
  ?max_domains
  ~summary
  ?readme
  param
  =
  async_or_error
    ~summary
    ?readme
    ?behave_nicely_in_pipeline
    ?extract_exn
    (Param.map param ~f:(fun main () ->
       main
         (Parallel_async.create
            (module Parallel_scheduler)
            (Parallel_scheduler.create ?max_domains ()))))
;;

module Staged = struct
  include Async.Command.Staged

  let parallel_async
    ?behave_nicely_in_pipeline
    ?extract_exn
    ?max_domains
    ~summary
    ?readme
    param
    =
    Staged.async
      ~summary
      ?readme
      ?behave_nicely_in_pipeline
      ?extract_exn
      (Param.map param ~f:(fun main () ->
         main
           (Parallel_async.create
              (module Parallel_scheduler)
              (Parallel_scheduler.create ?max_domains ()))))
  ;;

  let parallel_async_or_error
    ?behave_nicely_in_pipeline
    ?extract_exn
    ?max_domains
    ~summary
    ?readme
    param
    =
    Staged.async_or_error
      ~summary
      ?readme
      ?behave_nicely_in_pipeline
      ?extract_exn
      (Param.map param ~f:(fun main () ->
         main
           (Parallel_async.create
              (module Parallel_scheduler)
              (Parallel_scheduler.create ?max_domains ()))))
  ;;
end
