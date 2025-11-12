open! Base
open! Import

module Test_scheduler (Scheduler : Parallel.Scheduler.S) = struct
  let scheduler = Scheduler.create ()

  module Test_intf (Seq : Parallel.Sequence.S) = struct
    (* Adapted from base/test/test_sequence.ml *)
    module%test [@name "range symmetries"] _ = struct
      let basic parallel ~stride ~start ~stop ~start_n ~stop_n ~(result : int list) =
        let range = Seq.range ~stride ~start ~stop start_n stop_n in
        let list = Seq.to_list parallel range in
        let iarray = Seq.to_iarray parallel range in
        let iresult = Iarray.of_list result in
        List.equal Int.equal list result && Iarray.equal Int.equal iarray iresult
      ;;

      let test stride (start_n, start) (stop_n, stop) result =
        Scheduler.parallel scheduler ~f:(fun parallel ->
          basic parallel ~stride ~start ~stop ~start_n ~stop_n ~result
          && (* works for negative [start] and [stop] *)
          basic
            parallel
            ~stride:(-stride)
            ~start_n:(-start_n)
            ~stop_n:(-stop_n)
            ~start
            ~stop
            ~result:(List.map result ~f:(fun x -> -x)))
      ;;

      let%test _ = test 1 (3, `inclusive) (1, `exclusive) []
      let%test _ = test 1 (3, `inclusive) (3, `exclusive) []
      let%test _ = test 1 (3, `inclusive) (3, `inclusive) [ 3 ]
      let%test _ = test 1 (3, `inclusive) (4, `exclusive) [ 3 ]
      let%test _ = test 1 (3, `inclusive) (8, `exclusive) [ 3; 4; 5; 6; 7 ]
      let%test _ = test 3 (4, `inclusive) (10, `exclusive) [ 4; 7 ]
      let%test _ = test 3 (4, `inclusive) (11, `exclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (12, `exclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (13, `exclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (14, `exclusive) [ 4; 7; 10; 13 ]
      let%test _ = test (-1) (1, `inclusive) (3, `exclusive) []
      let%test _ = test (-1) (3, `inclusive) (3, `exclusive) []
      let%test _ = test (-1) (4, `inclusive) (3, `exclusive) [ 4 ]
      let%test _ = test (-1) (8, `inclusive) (3, `exclusive) [ 8; 7; 6; 5; 4 ]
      let%test _ = test (-3) (10, `inclusive) (4, `exclusive) [ 10; 7 ]
      let%test _ = test (-3) (10, `inclusive) (3, `exclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (2, `exclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (1, `exclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (0, `exclusive) [ 10; 7; 4; 1 ]
      let%test _ = test 1 (3, `exclusive) (1, `exclusive) []
      let%test _ = test 1 (3, `exclusive) (3, `exclusive) []
      let%test _ = test 1 (3, `exclusive) (4, `exclusive) []
      let%test _ = test 1 (3, `exclusive) (8, `exclusive) [ 4; 5; 6; 7 ]
      let%test _ = test 3 (4, `exclusive) (10, `exclusive) [ 7 ]
      let%test _ = test 3 (4, `exclusive) (11, `exclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (12, `exclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (13, `exclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (14, `exclusive) [ 7; 10; 13 ]
      let%test _ = test (-1) (1, `exclusive) (3, `exclusive) []
      let%test _ = test (-1) (3, `exclusive) (3, `exclusive) []
      let%test _ = test (-1) (4, `exclusive) (3, `exclusive) []
      let%test _ = test (-1) (8, `exclusive) (3, `exclusive) [ 7; 6; 5; 4 ]
      let%test _ = test (-3) (10, `exclusive) (4, `exclusive) [ 7 ]
      let%test _ = test (-3) (10, `exclusive) (3, `exclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (2, `exclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (1, `exclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (0, `exclusive) [ 7; 4; 1 ]
      let%test _ = test 1 (3, `inclusive) (1, `inclusive) []
      let%test _ = test 1 (3, `inclusive) (3, `inclusive) [ 3 ]
      let%test _ = test 1 (3, `inclusive) (4, `inclusive) [ 3; 4 ]
      let%test _ = test 1 (3, `inclusive) (8, `inclusive) [ 3; 4; 5; 6; 7; 8 ]
      let%test _ = test 3 (4, `inclusive) (10, `inclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (11, `inclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (12, `inclusive) [ 4; 7; 10 ]
      let%test _ = test 3 (4, `inclusive) (13, `inclusive) [ 4; 7; 10; 13 ]
      let%test _ = test 3 (4, `inclusive) (14, `inclusive) [ 4; 7; 10; 13 ]
      let%test _ = test (-1) (1, `inclusive) (3, `inclusive) []
      let%test _ = test (-1) (3, `inclusive) (3, `inclusive) [ 3 ]
      let%test _ = test (-1) (4, `inclusive) (3, `inclusive) [ 4; 3 ]
      let%test _ = test (-1) (8, `inclusive) (3, `inclusive) [ 8; 7; 6; 5; 4; 3 ]
      let%test _ = test (-3) (10, `inclusive) (4, `inclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (3, `inclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (2, `inclusive) [ 10; 7; 4 ]
      let%test _ = test (-3) (10, `inclusive) (1, `inclusive) [ 10; 7; 4; 1 ]
      let%test _ = test (-3) (10, `inclusive) (0, `inclusive) [ 10; 7; 4; 1 ]
      let%test _ = test 1 (3, `exclusive) (1, `inclusive) []
      let%test _ = test 1 (3, `exclusive) (3, `inclusive) []
      let%test _ = test 1 (3, `exclusive) (4, `inclusive) [ 4 ]
      let%test _ = test 1 (3, `exclusive) (8, `inclusive) [ 4; 5; 6; 7; 8 ]
      let%test _ = test 3 (4, `exclusive) (10, `inclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (11, `inclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (12, `inclusive) [ 7; 10 ]
      let%test _ = test 3 (4, `exclusive) (13, `inclusive) [ 7; 10; 13 ]
      let%test _ = test 3 (4, `exclusive) (14, `inclusive) [ 7; 10; 13 ]
      let%test _ = test (-1) (1, `exclusive) (3, `inclusive) []
      let%test _ = test (-1) (3, `exclusive) (3, `inclusive) []
      let%test _ = test (-1) (4, `exclusive) (3, `inclusive) [ 3 ]
      let%test _ = test (-1) (8, `exclusive) (3, `inclusive) [ 7; 6; 5; 4; 3 ]
      let%test _ = test (-3) (10, `exclusive) (4, `inclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (3, `inclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (2, `inclusive) [ 7; 4 ]
      let%test _ = test (-3) (10, `exclusive) (1, `inclusive) [ 7; 4; 1 ]
      let%test _ = test (-3) (10, `exclusive) (0, `inclusive) [ 7; 4; 1 ]
    end
  end

  module%test Test_without_length = Test_intf (Parallel.Sequence)
  module%test Test_with_length = Test_intf (Parallel.Sequence.With_length)
end

include Common.Test_schedulers (Test_scheduler)
