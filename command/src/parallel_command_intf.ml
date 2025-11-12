open! Core
open Core.Command

module Definitions = struct
  type 'a with_options = ?max_domains:int -> 'a
  type 'f command_fun = summary:string -> ?readme:(unit -> string) -> 'f Param.t -> t
  type 'r staged = ([ `Scheduler_started ] -> 'r) Staged.t
  type 'a with_async_options = 'a with_options Async.Command.with_options
end

module type Parallel_command = sig
  (** [Parallel_command] is {!Core.Command} with additional functions for working with
      Parallel *)

  open! Core

  include module type of struct
    include Definitions
  end

  (** [parallel] is like [Core.Command.basic], except the main function it expects takes a
      [Parallel_scheduler.t] instead of [unit].

      The [max_domains] argument is passed along to [Parallel_scheduler.create]. It
      defaults to the number of available cores (the value of
      [Domain.recommended_domain_count ()]) *)
  val parallel : (Parallel_scheduler.t -> unit) command_fun with_options

  (** [parallel_or_error] is like [Core.Command.basic_or_error], except the main function
      it expects takes a [Parallel_scheduler.t] instead of [unit]. *)
  val parallel_or_error
    : (Parallel_scheduler.t -> unit Or_error.t) command_fun with_options

  (** {1 Wrappers around {!Async.Command}}

      {b Usage example}

      {[
        open! Core
        open! Async

        let command =
          Parallel_command.parallel_async
            ~summary:"Do things in parallel"
            (let%map_open.Command () = return () in
             fun scheduler ->
               let%bind msg =
                 Parallel_async.parallel scheduler ~f:(fun par ->
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

  (** [parallel_async] is like [Async.Command.basic], except the main function it expects
      takes a [Parallel_async.t] instead of [unit]. *)
  val parallel_async
    : (Parallel_async.t -> unit Async.Deferred.t) command_fun with_async_options

  (** [parallel_async_or_error] is like [Async.Command.basic_or_error], except the main
      function it expects takes a [Parallel_async.t] instead of [unit]. *)
  val parallel_async_or_error
    : (Parallel_async.t -> unit Async.Deferred.Or_error.t) command_fun with_async_options

  (** Wrappers around {!Async.Command.Staged} which also provide a [Parallel_async.t] *)
  module Staged : sig
    val parallel_async
      : (Parallel_async.t -> unit Async.Deferred.t staged) command_fun with_async_options

    val parallel_async_or_error
      : (Parallel_async.t -> unit Async.Deferred.Or_error.t staged) command_fun
          with_async_options
  end
end
