@@ portable

open! Base
open! Import

(** The number of promotions to amortize per heartbeat. Default: 15 *)
val heartbeat_promotions : int

(** The heartbeat interval in microseconds. Default: 250 *)
val heartbeat_interval_us : int
