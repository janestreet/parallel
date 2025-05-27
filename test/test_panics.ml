open! Core
open! Import

type nothing = |

type scheduler =
  | Seq
  | Queue
  | Work_stealing

let () = Dynamic.set_root Backtrace.elide true

let create_incident () =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let exn = Capsule.Data.create (fun () -> Failure "Test") in
  let backtrace = Capsule.Data.create (fun () -> Backtrace.get ()) in
  Parallel.Panic.Incident.create_uncaught mutex exn backtrace
;;

let run_with_print sched ~f ~print_incident ~print_panic =
  let monitor = Parallel.Monitor.create_root () in
  Parallel.Monitor.on_panic monitor (fun incident ->
    printf "Monitor:\n%s\n" (print_incident incident));
  try
    match sched, is_runtime5 with
    | Seq, _ | Queue, false | Work_stealing, false ->
      let scheduler = Parallel.Scheduler.Sequential.create () in
      Parallel.Scheduler.Sequential.schedule scheduler ~monitor ~f
    | Queue, true ->
      let scheduler =
        (Parallel_scheduler_stack.create [@alert "-experimental"]) ~domains:2 ()
      in
      Parallel_scheduler_stack.schedule scheduler ~monitor ~f;
      Parallel_scheduler_stack.stop scheduler
    | Work_stealing, true ->
      let scheduler =
        (Parallel_scheduler_work_stealing.create [@alert "-experimental"]) ~domains:2 ()
      in
      Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f;
      Parallel_scheduler_work_stealing.stop scheduler
  with
  | Parallel.Panic panic ->
    printf "Top level:\n%s\n" (print_panic panic);
    Parallel.Panic.iter_incidents panic (fun incident ->
      printf "Panic contains:\n%s\n" (print_incident incident))
;;

let run sched ~f =
  run_with_print
    sched
    ~f
    ~print_incident:Parallel.Panic.Incident.to_string_hum
    ~print_panic:Parallel.Panic.to_string_hum;
  print_endline "-----\n";
  run_with_print
    sched
    ~f
    ~print_incident:(fun incident ->
      Sexp.to_string_hum (Parallel.Panic.Incident.sexp_of_t incident) ^ "\n")
    ~print_panic:(fun panic -> Sexp.to_string_hum (Parallel.Panic.sexp_of_t panic) ^ "\n")
;;

let%expect_test "panic" =
  run Seq ~f:(fun parallel -> Parallel.panic parallel (create_incident ()));
  [%expect
    {|
    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure Test)
      <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure Test))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "fork panic" =
  run Seq ~f:(fun parallel ->
    match
      Parallel.fork_join
        parallel
        [ (fun parallel -> Parallel.panic parallel (create_incident ())) ]
    with
    | [ (_ : nothing) ] -> .);
  [%expect
    {|
    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure Test)
        <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure Test))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "for panic" =
  run Seq ~f:(fun parallel ->
    Parallel.for_ parallel ~start:0 ~stop:1 ~f:(fun parallel _ ->
      Parallel.panic parallel (create_incident ())));
  [%expect
    {|
    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure Test)
        <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure Test))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "fork panic multiple" =
  run Seq ~f:(fun parallel ->
    match
      Parallel.fork_join
        parallel
        [ (fun parallel -> Parallel.panic parallel (create_incident ()))
        ; (fun parallel -> Parallel.panic parallel (create_incident ()))
        ; (fun parallel -> Parallel.panic parallel (create_incident ()))
        ]
    with
    | [ (_ : nothing); (_ : nothing); (_ : nothing) ] -> .);
  [%expect
    {|
    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Monitor:
    (Failure Test)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure Test)
        <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure Test)
        <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure Test)
        <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    Panic contains:
    (Failure Test)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure Test))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))
       (Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure Test))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))
       (Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure Test))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure Test))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "seq raise" =
  run Seq ~f:(fun parallel ->
    let _, _ =
      Parallel.fork_join2
        parallel
        (fun _ -> print_endline "hello\n")
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    hello

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure fail)
      <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    hello

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure fail))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "seq raise2" =
  run Seq ~f:(fun parallel ->
    let _, _ =
      Parallel.fork_join2
        parallel
        (fun _ ->
          match (failwith "fail1" : nothing) with
          | _ -> .)
        (fun _ ->
          match (failwith "fail2" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    Monitor:
    (Failure fail1)
    <backtrace elided in test>

    Monitor:
    (Failure fail2)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure fail1)
      <backtrace elided in test>
      Panicked due to incident:
      (Failure fail2)
      <backtrace elided in test>

    Panic contains:
    (Failure fail1)
    <backtrace elided in test>

    Panic contains:
    (Failure fail2)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail1))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail2))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure fail1))
         (backtrace ("<backtrace elided in test>"))))
       (Incident
        ((id "<id elided in test>") (exn (Failure fail2))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail1))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail2))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "for raise" =
  run Seq ~f:(fun parallel ->
    Parallel.for_ parallel ~start:0 ~stop:2 ~f:(fun _ i ->
      if i = 0 then print_endline "hello\n" else failwith "fail"));
  [%expect
    {|
    hello

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure fail)
      <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    hello

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure fail))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "queue raise1" =
  run Queue ~f:(fun _ -> failwith "fail");
  [%expect
    {|
    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to incident:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Incident
     ((id "<id elided in test>") (exn (Failure fail))
      (backtrace ("<backtrace elided in test>"))))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "queue raise" =
  run Queue ~f:(fun parallel ->
    let _, _ =
      Parallel.fork_join2
        parallel
        (fun _ -> print_endline "hello\n")
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    hello

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure fail)
      <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    hello

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure fail))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "queue raise2" =
  run Queue ~f:(fun parallel ->
    let _, _ =
      Parallel.fork_join2
        parallel
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to incident:
      (Failure fail)
      <backtrace elided in test>
      Panicked due to incident:
      (Failure fail)
      <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Incident
        ((id "<id elided in test>") (exn (Failure fail))
         (backtrace ("<backtrace elided in test>"))))
       (Incident
        ((id "<id elided in test>") (exn (Failure fail))
         (backtrace ("<backtrace elided in test>"))))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "queue nested raise" =
  run Queue ~f:(fun parallel ->
    let (_, _), (_, _) =
      Parallel.fork_join2
        parallel
        (fun parallel ->
          Parallel.fork_join2
            parallel
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .)
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .))
        (fun parallel -> Parallel.fork_join2 parallel (fun _ -> ()) (fun _ -> ()))
    in
    ());
  [%expect
    {|
    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))
          (Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "queue raise4" =
  run Queue ~f:(fun parallel ->
    let (_, _), (_, _) =
      Parallel.fork_join2
        parallel
        (fun parallel ->
          Parallel.fork_join2
            parallel
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .)
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .))
        (fun parallel ->
          Parallel.fork_join2
            parallel
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .)
            (fun _ ->
              match (failwith "fail" : nothing) with
              | _ -> .))
    in
    ());
  [%expect
    {|
    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Monitor:
    (Failure fail)
    <backtrace elided in test>

    Top level:
    Panicked due to inner panics:
    <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>
      Panicked due to inner panics:
      <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>
        Panicked due to incident:
        (Failure fail)
        <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    Panic contains:
    (Failure fail)
    <backtrace elided in test>

    -----

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Monitor:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Top level:
    (Panic
     (children
      ((Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))
          (Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))
       (Panic
        (children
         ((Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))
          (Incident
           ((id "<id elided in test>") (exn (Failure fail))
            (backtrace ("<backtrace elided in test>"))))))
        (backtrace ("<backtrace elided in test>")))))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))

    Panic contains:
    ((id "<id elided in test>") (exn (Failure fail))
     (backtrace ("<backtrace elided in test>")))
    |}]
;;

let%expect_test "of_incident in on_panic" =
  run_with_print
    Seq
    ~f:(fun _ -> failwith "failure")
    ~print_incident:(fun incident ->
      let panic = Parallel.Panic.of_incident incident in
      Parallel.Panic.to_string_hum panic)
    ~print_panic:Parallel.Panic.to_string_hum;
  [%expect
    {|
    Monitor:
    Panicked due to incident:
    (Failure failure)
    <backtrace elided in test>

    Top level:
    Panicked due to incident:
    (Failure failure)
    <backtrace elided in test>

    Panic contains:
    Panicked due to incident:
    (Failure failure)
    <backtrace elided in test>
    |}]
;;
