@@ portable

open! Base

(* $MDX part-begin=image *)
type t : mutable_data

val width : t @ contended -> int
val height : t @ contended -> int
val of_array : float array -> width:int -> height:int -> t
val get : t @ shared -> x:int -> y:int -> float
val set : t -> x:int -> y:int -> float -> unit

(* $MDX part-end *)

val load : string -> t
val save : t @ shared -> string -> unit
