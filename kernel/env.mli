@@ portable

open! Base
open! Import

(** The maximum number of jobs to promote during a heartbeat. Up to three additional jobs
    may be promoted in a given heartbeat, due to the implementation of the job queue.
    Default: 100 *)
val max_promotions : int

(** The heartbeat interval in microseconds. Default: 10 *)
val heartbeat_interval_us : int
