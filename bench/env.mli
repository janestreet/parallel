@@ portable

open! Base

(** The number of domains on which to run parallel tasks. *)
val domains : int

(** The granularity at which to break up parallel array tasks. *)
val grain : int

(** The length of arrays for parallel array benchmarks. *)
val length : int
