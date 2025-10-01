@@ portable

open! Base

type ('a, 'b) t : (value_or_null & value_or_null) mod everything with 'a with 'b

val none : unit -> ('a, 'b) t @ portable
val some : 'a -> 'b -> ('a, 'b) t

module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none : ('a, 'b) t @ contended local -> bool

    external unsafe_value
      :  (('a, 'b) t[@local_opt])
      -> (#('a * 'b)[@local_opt])
      = "%identity"
  end
end
