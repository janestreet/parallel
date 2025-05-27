open! Base
open! Import

let sprintf ~indent fmt =
  Printf.ksprintf
    (fun s ->
      let b = Bytes.make (indent + String.length s) ' ' in
      Bytes.From_string.blito ~src:s ~dst:b ~dst_pos:indent ();
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b)
    fmt
;;

let raw_backtrace_to_lines backtrace =
  Backtrace.to_string backtrace |> String.strip |> String.split_lines
;;

external caml_fatal_error : string -> 'a @ portable @@ portable = "caml_fatal_error"

let abort_nested_panic () = caml_fatal_error "Panicked in on_panic, terminating program."

module Incident = struct
  module Id : sig @@ portable
    type t : immediate

    val equal : [%equal: t]
    val sexp_of_t : [%sexp_of: t]
    val next : unit -> t
  end = struct
    type t = int [@@deriving equal]

    let sexp_of_t t =
      match Dynamic.get Backtrace.elide with
      | true -> [%sexp "<id elided in test>"]
      | false -> [%sexp (t : int)]
    ;;

    let next =
      let id = Atomic.make 0 in
      fun () -> Atomic.fetch_and_add id 1
    ;;
  end

  type 'k inner : value mod contended portable =
    { id : Id.t
    ; mutex : 'k Capsule.Mutex.t
    ; exn : (exn, 'k) Capsule.Data.t
    ; backtrace : (Backtrace.t, 'k) Capsule.Data.t
    }

  type t : value mod contended portable = P : 'k inner -> t [@@unboxed]

  let create_uncaught mutex exn backtrace = P { id = Id.next (); mutex; exn; backtrace }
  let equal (P { id = id0; _ }) (P { id = id1; _ }) = Id.equal id0 id1

  let exn_to_string (P { mutex; exn; _ }) =
    (Capsule.Mutex.with_lock mutex ~f:(fun password ->
       Capsule.Data.extract exn ~password ~f:(fun exn -> { aliased = Exn.to_string exn })
       [@nontail]))
      .aliased
  ;;

  let sexp_of_t (P { id; mutex; exn; backtrace }) =
    (Capsule.Mutex.with_lock mutex ~f:(fun password ->
       Capsule.Data.extract
         (Capsule.Data.both exn backtrace)
         ~password
         ~f:(fun (exn, backtrace) ->
           { aliased = [%message (id : Id.t) (exn : Exn.t) (backtrace : Backtrace.t)] })
       [@nontail]))
      .aliased
  ;;

  let to_string t = Sexp.to_string (sexp_of_t t)

  let to_string_hum (P { mutex; exn; backtrace; _ }) =
    (Capsule.Mutex.with_lock mutex ~f:(fun password ->
       Capsule.Data.extract
         (Capsule.Data.both exn backtrace)
         ~password
         ~f:(fun (exn, backtrace) ->
           let lines = Exn.to_string exn :: raw_backtrace_to_lines backtrace in
           { aliased = String.concat_lines lines })
       [@nontail]))
      .aliased
  ;;
end

module Backtrace = struct
  type 'k t =
    | None
    | Some of
        { mutex : 'k Capsule.Mutex.t
        ; backtrace : (Backtrace.t, 'k) Capsule.Data.t
        }

  let sexp_of_t _ = function
    | None -> Sexp.List []
    | Some { mutex; backtrace } ->
      (Capsule.Mutex.with_lock mutex ~f:(fun password ->
         Capsule.Data.extract backtrace ~password ~f:(fun backtrace ->
           { aliased = Backtrace.sexp_of_t backtrace })
         [@nontail]))
        .aliased
  ;;

  let to_string ~indent mutex backtrace =
    (Capsule.Mutex.with_lock mutex ~f:(fun password ->
       Capsule.Data.extract backtrace ~password ~f:(fun backtrace ->
         raw_backtrace_to_lines backtrace
         |> List.map ~f:(sprintf ~indent "%s")
         |> String.concat_lines
         |> fun aliased -> { aliased })
       [@nontail]))
      .aliased
  ;;
end

type t : value mod contended portable =
  | Incident of Incident.t
  | Panic :
      { children : t list
      ; backtrace : 'k Backtrace.t
      }
      -> t
[@@unsafe_allow_any_mode_crossing] [@@deriving sexp_of]

let with_backtrace t mutex backtrace =
  match t with
  | Panic { children; backtrace = None } ->
    Panic { children; backtrace = Some { mutex; backtrace } }
  | Incident _ | Panic _ ->
    Panic { children = [ t ]; backtrace = Some { mutex; backtrace } }
;;

let join children = Panic { children; backtrace = None }

let rec iter_incidents t f =
  match t with
  | Incident incident -> f incident
  | Panic { children; _ } ->
    List.iter children ~f:(fun t -> iter_incidents t f) [@nontail]
;;

let to_string t = Sexp.to_string (sexp_of_t t)

let rec to_string_hum ~indent panic =
  match panic with
  | Incident (P { mutex; backtrace; _ } as incident) ->
    let context = Backtrace.to_string ~indent mutex backtrace in
    let incident = sprintf ~indent "%s" (Incident.exn_to_string incident) in
    sprintf ~indent "Panicked due to incident:\n%s\n%s" incident context
  | Panic { children; backtrace } ->
    let context =
      match backtrace with
      | Some { mutex; backtrace } -> Backtrace.to_string ~indent mutex backtrace
      | None -> ""
    in
    let children =
      List.map children ~f:(to_string_hum ~indent:(indent + 2)) |> String.concat
    in
    sprintf ~indent "Panicked due to inner panics:\n%s%s" context children
;;

let to_string_hum = to_string_hum ~indent:0

exception Panic of t [@@deriving sexp_of]

module Monitor = struct
  type t : value mod contended portable =
    | P :
        { parent : t option
        ; mutex : 'k Capsule.Mutex.t
        ; reports : ((Incident.t -> unit) portable list ref, 'k) Capsule.Data.t
        }
        -> t
  [@@unsafe_allow_any_mode_crossing]

  let create_aux ~parent =
    let (P key) = Capsule.create () in
    let mutex = Capsule.Mutex.create key in
    let reports = Capsule.Data.create (fun () -> ref []) in
    P { parent; mutex; reports }
  ;;

  let create ~parent = create_aux ~parent:(Some parent)
  let create_root () = create_aux ~parent:None

  let rec panic (P { parent; mutex; reports }) (incident : Incident.t) =
    Capsule.Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Data.iter reports ~password ~f:(fun reports ->
        List.iter !reports ~f:(fun { portable } ->
          try portable incident with
          | _ -> abort_nested_panic ())
        [@nontail])
      [@nontail]);
    Option.iter parent ~f:(fun parent -> panic parent incident) [@nontail]
  ;;

  let on_panic (P { mutex; reports; _ }) (report : Incident.t -> unit) =
    Capsule.Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Data.iter reports ~password ~f:(fun reports ->
        reports := { portable = report } :: !reports)
      [@nontail])
    [@nontail]
  ;;
end

let of_incident incident = Incident incident

module Result = struct
  type panic = t

  module Ok_or_exn = struct
    type ('a, 'k) t =
      | Ok of ('a, 'k) Capsule.Data.t @@ aliased global
      | Exn of (exn, 'k) Capsule.Data.t @@ aliased global
  end

  type 'a t : value mod contended portable =
    | Ok : ('a, 'k) Capsule.Data.t @@ aliased global * 'k Capsule.Key.t @@ global -> 'a t
    | Panic of panic @@ aliased global
  [@@unsafe_allow_any_mode_crossing]

  let[@inline never] report_incident monitor key exn = exclave_
    let mutex = Capsule.Mutex.create key in
    let backtrace = Capsule.Data.create Base.Backtrace.Exn.most_recent in
    let panic =
      (Capsule.Mutex.with_lock mutex ~f:(fun password ->
         { many =
             { aliased =
                 Capsule.access ~password ~f:(fun access ->
                   match (Capsule.Data.unwrap ~access exn : exn) with
                   | Panic panic -> with_backtrace panic mutex backtrace
                   | _ ->
                     let incident = Incident.create_uncaught mutex exn backtrace in
                     Incident incident)
             }
         }))
        .many
        .aliased
    in
    (match panic with
     | Incident incident -> Monitor.panic monitor incident
     | Panic _ -> ());
    Panic panic
  ;;

  let[@inline] handle_panics_and_report_exceptions monitor f = exclave_
    let (P key) = Capsule.create () in
    let result, key =
      Capsule.Key.access_local key ~f:(fun [@inline] access ->
        exclave_
        let res =
          try Ok_or_exn.Ok (Capsule.Data.wrap ~access (f ())) with
          | exn -> Exn (Capsule.Data.wrap ~access exn)
        in
        { aliased = { many = res } })
    in
    let key = Capsule.Key.globalize_unique key in
    match result.aliased.many with
    | Ok a -> Ok (a, key)
    | Exn exn -> report_incident monitor key exn [@nontail]
  ;;

  let globalize : 'a t @ local unique -> 'a t @ unique = function
    | Ok (a, key) -> Ok (a, key)
    | Panic p -> Panic p
  ;;
end

let[@inline] handle_panics_and_report_exceptions monitor ~on_panic ~f =
  match Result.handle_panics_and_report_exceptions monitor f with
  | Ok (a, key) ->
    let access = Capsule.Key.destroy key in
    Capsule.Data.unwrap ~access a
  | Panic panic ->
    (try on_panic panic with
     | _ -> abort_nested_panic ())
;;
