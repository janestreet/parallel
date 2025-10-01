@@ portable

open! Base

(** The maximum number of domains on which to run parallel tasks. *)
val max_domains : int

(** The granularity at which to break up parallel array tasks. *)
val grain : int

(** The length of arrays for parallel array benchmarks. *)
val length : int

(** The number of eager promotions. *)
val eager : int
