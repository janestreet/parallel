open! Base
open! Import
include Parallel_kernel0.Scheduler

let root t ~f =
  let (P key) = Capsule.create () in
  Promise.fiber
    (Promise.start ())
    (fun parallel ->
      f parallel;
      exclave_ Ok (Capsule.Data.inject (), key))
    ~scheduler:t
;;

let[@inline never] promote (t : t) ~(queue : Runqueue.t) =
  let n = ref 0 in
  while queue.heartbeats < Atomic.get Parallel_kernel1.heartbeat_counter do
    Runqueue.promote queue ~f:(fun job promise ->
      t.#promote (Promise.fiber promise job ~scheduler:t);
      Int.incr n);
    queue.heartbeats <- queue.heartbeats + 1
  done;
  t.#wake ~n:!n
;;

module One_job = struct
  type 'a t =
    #{ job : 'a Parallel_kernel1.Job.t
     ; promise : 'a Promise.t @@ many
     }

  let[@inline] await #{ job; promise } parallel = exclave_
    Promise.await promise job parallel
  ;;
end

let[@inline never] promote_one (t : t) ~f =
  let promise = Promise.start () in
  let job = Parallel_kernel1.Job.wrap f in
  (* Safe because [f] is applied by either the fiber or the caller, never both.
     This is enforced by the behavior of [Promise.apply] and [Promise.await]. *)
  let job = (Obj.magic_many [@mode uncontended portable aliased]) job in
  t.#promote (Promise.fiber promise job ~scheduler:t);
  t.#wake ~n:1;
  #{ One_job.job; promise }
;;
