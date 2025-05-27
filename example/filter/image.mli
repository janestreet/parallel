@@ portable

open! Base

type t : value mod portable

val load : string -> t
val save : t @ shared -> string -> unit
val width : t @ contended -> int
val height : t @ contended -> int
val of_array : float array -> width:int -> height:int -> t
val get : t @ shared -> x:int -> y:int -> float
val set : t -> x:int -> y:int -> float -> unit
