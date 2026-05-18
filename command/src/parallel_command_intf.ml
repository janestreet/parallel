open! Core
open Core.Command

module Definitions = struct
  type 'a with_options = ?max_workers:int -> 'a
  type 'a with_async_options = (?max_workers:int -> 'a) Async.Command.with_options
  type 'f command_fun = summary:string -> ?readme:(unit -> string) -> 'f Param.t -> t
  type 'r staged = ([ `Scheduler_started ] -> 'r) Staged.t
end

module type Parallel_command = sig
  (** [Parallel_command] is {!Core.Command} with additional functions for working with
      Parallel *)

  open! Core

  include module type of struct
    include Definitions
  end

  (** [parallel] is like [Core.Command.basic], except the main function it expects takes a
      [Parallel.t] instead of [unit].

      The [max_workers] argument defaults to the number of available cores (the value of
      [Domain.recommended_domain_count ()]). *)
  val parallel : (Parallel.t @ local -> unit) command_fun with_options

  (** [parallel_or_error] is like [Core.Command.basic_or_error], except the main function
      it expects takes a [Parallel.t] instead of [unit]. *)
  val parallel_or_error : (Parallel.t @ local -> unit Or_error.t) command_fun with_options

  (** [concurrent] is like [Core.Command.basic], except the main function it expects takes
      a [Parallel.t Concurrent.t] instead of [unit].

      The [max_workers] argument defaults to the number of available cores (the value of
      [Domain.recommended_domain_count ()]). *)
  val concurrent : (Parallel.t Concurrent.t @ local -> unit) command_fun with_options

  (** [concurrent_or_error] is like [Core.Command.basic_or_error], except the main
      function it expects takes a [Parallel.t Concurrent.t] instead of [unit]. *)
  val concurrent_or_error
    : (Parallel.t Concurrent.t @ local -> unit Or_error.t) command_fun with_options

  (** {1 Wrappers around {!Async.Command}}

      {b Usage example}

      {[
        open! Core
        open! Async

        let command =
          Parallel_command.async
            ~summary:"Do things in parallel"
            (let%map_open.Command () = return () in
             fun scheduler ->
               let%bind msg =
                 Concurrent_in_async.spawn_join scheduler ~f:(fun par _ ->
                   let #(x, y) =
                     Parallel.fork_join2
                       par
                       (fun (_ : Parallel.t) -> "hello from ")
                       (fun (_ : Parallel.t) -> "parallel ")
                   in
                   x ^ y)
               in
               print_endline msg;
               return ())
        ;;
      ]} *)

  (** [async] is like [Async.Command.basic], except the main function it expects takes a
      [Parallel.t Concurrent.Scheduler.t] instead of [unit]. *)
  val async
    : (Parallel.t Concurrent.Scheduler.t @ portable -> unit Async.Deferred.t) command_fun
        with_async_options

  (** [async_or_error] is like [Async.Command.basic_or_error], except the main function it
      expects takes a [Parallel.t Concurrent.Scheduler.t] instead of [unit]. *)
  val async_or_error
    : (Parallel.t Concurrent.Scheduler.t @ portable -> unit Async.Deferred.Or_error.t)
        command_fun
        with_async_options

  (** Wrappers around {!Async.Command.Staged} which also provide a
      [Parallel.t Concurrent.Scheduler.t] *)
  module Staged : sig
    val async
      : (Parallel.t Concurrent.Scheduler.t @ portable -> unit Async.Deferred.t staged)
          command_fun
          with_async_options

    val async_or_error
      : (Parallel.t Concurrent.Scheduler.t @ portable
         -> unit Async.Deferred.Or_error.t staged)
          command_fun
          with_async_options
  end
end
