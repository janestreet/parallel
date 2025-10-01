open! Base

type ('a, 'b) t =
  #{ a : 'a or_null
   ; b : 'b or_null
   }

let[@inline] none () = #{ a = Null; b = Null }
let[@inline] some a b = #{ a = This a; b = This b }

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@inline] is_none = function
      | #{ a = Null; _ } -> true
      | _ -> false
    ;;

    external unsafe_value
      :  (('a, 'b) t[@local_opt])
      -> (#('a * 'b)[@local_opt])
      @@ portable
      = "%identity"
  end
end
