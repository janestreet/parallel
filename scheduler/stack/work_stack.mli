@@ portable

open! Base

type t : value mod contended portable

val create : unit -> t
val work : t -> break:(unit -> bool) @ local portable -> unit
val wake : t -> unit
val push : t -> f:(unit -> unit) @ once portable -> unit
