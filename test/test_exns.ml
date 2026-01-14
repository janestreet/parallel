open! Core
open! Import
open Expect_test_helpers_core

type nothing = |

type scheduler =
  | Seq
  | Work_stealing

let never = Await.Terminator.never

let run sched ~f =
  try
    match sched with
    | Seq ->
      let scheduler = Parallel.Scheduler.Sequential.create () in
      Parallel.Scheduler.Sequential.parallel scheduler ~f
    | Work_stealing ->
      let scheduler = Parallel_scheduler.create () in
      Parallel_scheduler.parallel scheduler ~f;
      Parallel_scheduler.stop scheduler
  with
  | exn -> printf "Top level: %s\n" (Exn.to_string exn)
;;

let run_concurrent ~terminator ~f =
  let scheduler = Parallel_scheduler.create () in
  (try Parallel_scheduler.concurrent scheduler ~terminator ~f [@nontail] with
   | exn -> printf "Top level: %s\n" (Exn.to_string exn));
  Parallel_scheduler.stop scheduler
;;

let run_concurrent_scheduler ~f =
  let scheduler = Parallel_scheduler.create () in
  let concurrent = Parallel_scheduler.Expert.scheduler scheduler in
  (try f concurrent with
   | exn -> printf "Top level: %s\n" (Exn.to_string exn));
  Parallel_scheduler.stop scheduler
;;

let%expect_test "raise" =
  run Seq ~f:(fun _ -> failwith "fail");
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "fork raise" =
  run Seq ~f:(fun parallel ->
    match Parallel.fork_join parallel [ (fun _ -> failwith "fail") ] with
    | [ (_ : nothing) ] -> .);
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "for raise" =
  run Seq ~f:(fun parallel ->
    Parallel.for_ parallel ~start:0 ~stop:1 ~f:(fun _ i ->
      failwith ("fail" ^ Int.to_string i)));
  [%expect {| Top level: (Failure fail0) |}]
;;

let%expect_test "fork raise multiple" =
  run Seq ~f:(fun parallel ->
    match
      Parallel.fork_join
        parallel
        [ (fun _ -> failwith "fail0")
        ; (fun _ -> failwith "fail1")
        ; (fun _ -> failwith "fail2")
        ]
    with
    | [ (_ : nothing); (_ : nothing); (_ : nothing) ] -> .);
  [%expect {| Top level: (Failure fail0) |}]
;;

let%expect_test "seq raise" =
  run Seq ~f:(fun parallel ->
    let #(_, _) =
      Parallel.fork_join2
        parallel
        (fun _ -> print_endline "hello")
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    hello
    Top level: (Failure fail)
    |}]
;;

let%expect_test "seq raise2" =
  run Seq ~f:(fun parallel ->
    let #(_, _) =
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
  [%expect {| Top level: (Failure fail1) |}]
;;

let%expect_test "for raise" =
  run Seq ~f:(fun parallel ->
    Parallel.for_ parallel ~start:0 ~stop:2 ~f:(fun _ i ->
      if i = 0 then print_endline "hello" else failwith "fail"));
  [%expect
    {|
    hello
    Top level: (Failure fail)
    |}]
;;

let%expect_test "ws raise1" =
  run Work_stealing ~f:(fun _ -> failwith "fail");
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "ws raise" =
  run Work_stealing ~f:(fun parallel ->
    let #(_, _) =
      Parallel.fork_join2
        parallel
        (fun _ -> print_endline "hello")
        (fun _ ->
          match (failwith "fail" : nothing) with
          | _ -> .)
    in
    ());
  [%expect
    {|
    hello
    Top level: (Failure fail)
    |}]
;;

let%expect_test "ws raise2" =
  run Work_stealing ~f:(fun parallel ->
    let #(_, _) =
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
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "ws nested raise" =
  run Work_stealing ~f:(fun parallel ->
    let #(_, _) =
      Parallel.fork_join2
        parallel
        (fun parallel ->
          let #((), ()) =
            Parallel.fork_join2
              parallel
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
          in
          ())
        (fun parallel ->
          let #((), ()) = Parallel.fork_join2 parallel (fun _ -> ()) (fun _ -> ()) in
          ())
    in
    ());
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "ws raise4" =
  run Work_stealing ~f:(fun parallel ->
    let #(_, _) =
      Parallel.fork_join2
        parallel
        (fun parallel ->
          let #((), ()) =
            Parallel.fork_join2
              parallel
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
          in
          ())
        (fun parallel ->
          let #((), ()) =
            Parallel.fork_join2
              parallel
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
              (fun _ ->
                match (failwith "fail" : nothing) with
                | _ -> .)
          in
          ())
    in
    ());
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "array init" =
  let open Parallel.Arrays in
  run Seq ~f:(fun parallel ->
    let _ : _ = Array.init parallel 1 ~f:(fun _ -> failwith "fail") in
    ());
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "array map" =
  let open Parallel.Arrays in
  run Seq ~f:(fun parallel ->
    let array = Array.init parallel 1 ~f:(fun _ _ -> 0) in
    let _ : _ = Array.map parallel array ~f:(fun _ _ -> failwith "fail") in
    ());
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "array map2" =
  let open Parallel.Arrays in
  run Seq ~f:(fun parallel ->
    let array = Array.init parallel 1 ~f:(fun _ _ -> 0) in
    let _ : _ = Array.map2_exn parallel array array ~f:(fun _ _ _ -> failwith "fail") in
    ());
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent" =
  run_concurrent ~terminator:never ~f:(fun _concurrent -> failwith "fail");
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent scope" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun _spawn -> failwith "fail"));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent terminated" =
  let wait : unit Await.Ivar.t = Await.Ivar.create () in
  Await.Terminator.with_ (fun terminator ->
    Await.Terminator.source terminator
    |> Or_null.iter ~f:Await.Terminator.Source.terminate;
    run_concurrent ~terminator ~f:(fun concurrent ->
      Await.Ivar.read (Concurrent.await concurrent) wait [@nontail])
    [@nontail]);
  [%expect {| Top level: (Terminated) |}]
;;

let%expect_test "concurrent task" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope _parallel _concurrent -> failwith "fail")));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent task terminated" =
  let wait = Await.Ivar.create () in
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope _parallel concurrent ->
        Await.Ivar.read (Concurrent.await concurrent) wait [@nontail];
        failwith "unreachable");
      Concurrent.spawn spawn ~f:(fun _scope _parallel _concurrent -> failwith "fail")));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent parallel task terminated" =
  let wait : unit Await.Ivar.t = Await.Ivar.create () in
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope parallel concurrent ->
        let #((), ()) =
          Parallel.Biased.fork_join2
            parallel
            (fun _ -> Await.Ivar.read (Concurrent.await concurrent) wait [@nontail])
            (fun _ -> ())
        in
        ());
      Concurrent.spawn spawn ~f:(fun _scope _parallel _concurrent -> failwith "fail")));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent scheduler terminated" =
  let wait = Await.Ivar.create () in
  let scope =
    Await.Scope.Global.create () ~on_exit:(fun _scope maybe_exn ->
      Or_null.iter maybe_exn ~f:(fun (exn, _bt) ->
        printf "Uncaught exn: %s\n" (Exn.to_string exn)))
  in
  run_concurrent_scheduler ~f:(fun scheduler ->
    Concurrent.Scheduler.spawn
      scheduler
      scope
      (Concurrent.task (fun _scope _parallel concurrent ->
         Await.Ivar.read (Concurrent.await concurrent) wait;
         failwith "unreachable"));
    Concurrent.Scheduler.spawn
      scheduler
      scope
      (Concurrent.task (fun _scope _parallel _concurrent -> failwith "fail")));
  [%expect {| Uncaught exn: (Failure fail) |}]
;;

let%expect_test "concurrent parallel task" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
        let #((), ()) =
          Parallel.fork_join2
            parallel
            (fun _ -> failwith "fail")
            (fun _ -> failwith "fail")
        in
        ())));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent into scope" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun scope _parallel concurrent ->
        let spawn = Concurrent.into_scope concurrent scope in
        Concurrent.spawn spawn ~f:(fun _ -> failwith "fail") [@nontail])));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent nested scope" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope _parallel concurrent ->
        Concurrent.with_scope concurrent () ~f:(fun spawn ->
          Concurrent.spawn spawn ~f:(fun _ -> failwith "fail") [@nontail]))));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent parallel nested" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun _scope parallel concurrent ->
        let #((), ()) =
          Parallel.Biased.fork_join2
            parallel
            (fun _ ->
              Concurrent.with_scope concurrent () ~f:(fun spawn ->
                Concurrent.spawn spawn ~f:(fun _scope parallel _concurrent ->
                  let #((), ()) =
                    Parallel.fork_join2
                      parallel
                      (fun _ -> failwith "fail")
                      (fun _ -> failwith "fail")
                  in
                  ())))
            (fun _ -> failwith "fail")
        in
        ())));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent parallel into_scope" =
  run_concurrent ~terminator:never ~f:(fun concurrent ->
    Concurrent.with_scope concurrent () ~f:(fun spawn ->
      Concurrent.spawn spawn ~f:(fun scope parallel concurrent ->
        let #((), ()) =
          Parallel.Biased.fork_join2
            parallel
            (fun _ ->
              let spawn = Concurrent.into_scope concurrent scope in
              Concurrent.spawn spawn ~f:(fun _scope _parallel _concurrent ->
                failwith "fail")
              [@nontail])
            (fun _ -> ())
        in
        ())));
  [%expect {| Top level: (Failure fail) |}]
;;

let%expect_test "concurrent scheduler" =
  let scope =
    Await.Scope.Global.create () ~on_exit:(fun _scope maybe_exn ->
      Or_null.iter maybe_exn ~f:(fun (exn, _bt) ->
        printf "Uncaught exn: %s\n" (Exn.to_string exn)))
  in
  run_concurrent_scheduler ~f:(fun scheduler ->
    Concurrent.Scheduler.spawn
      scheduler
      scope
      (Concurrent.task (fun _scope _parallel _concurrent -> failwith "fail")));
  [%expect {| Uncaught exn: (Failure fail) |}]
;;

let%expect_test "concurrent scheduler parallel" =
  let scope =
    Await.Scope.Global.create () ~on_exit:(fun _scope maybe_exn ->
      Or_null.iter maybe_exn ~f:(fun (exn, _bt) ->
        printf "Uncaught exn: %s\n" (Exn.to_string exn)))
  in
  run_concurrent_scheduler ~f:(fun scheduler ->
    Concurrent.Scheduler.spawn
      scheduler
      scope
      (Concurrent.task (fun _scope parallel _concurrent ->
         let #((), ()) =
           Parallel.fork_join2
             parallel
             (fun _ -> failwith "fail")
             (fun _ -> failwith "fail")
         in
         ())));
  (* inner panics are reported to the monitor *)
  [%expect {| Uncaught exn: (Failure fail) |}]
;;
