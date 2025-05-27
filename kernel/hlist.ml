open! Base
open! Import

module type T1 = sig
  type 'a t
end

module Gen (T : T1) = struct
  type _ t =
    | [] : unit t
    | ( :: ) : 'a T.t * 'b t -> ('a * 'b) t
end

module Id = struct
  type 'a t = 'a
end

include Gen (Id)
