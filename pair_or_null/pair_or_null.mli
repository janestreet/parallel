@@ portable

open! Base

type ('a
     , 'b)
     t :
     (value_or_null & value_or_null)
     with 'a @@ contended portable
     with 'b @@ contended portable

val none : unit -> ('a, 'b) t @ contended portable

val some
  :  'a @ contended portable
  -> 'b @ contended portable
  -> ('a, 'b) t @ contended portable

module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none : ('a, 'b) t @ contended local -> bool

    external unsafe_value
      :  (('a, 'b) t[@local_opt]) @ contended portable
      -> (#('a * 'b)[@local_opt]) @ contended portable
      @@ portable
      = "%identity"
  end
end
