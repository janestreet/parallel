open! Base

type ('a, 'b) t =
  #{ a : 'a or_null @@ contended portable
   ; b : 'b or_null @@ contended portable
   }
[@@warning "-69"]

let[@inline] none () = #{ a = Null; b = Null }
let[@inline] some a b = #{ a = This a; b = This b }

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@inline] is_none = function
      | #{ a = Null; _ } -> true
      | _ -> false
    ;;

    external unsafe_value
      :  (('a, 'b) t[@local_opt]) @ contended portable
      -> (#('a * 'b)[@local_opt]) @ contended portable
      @@ portable
      = "%identity"
  end
end
