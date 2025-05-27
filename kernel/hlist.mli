open! Base
open! Import

(** Types with a single parameter. *)
module type T1 = sig
  type 'a t
end

module Gen (T : T1) : sig
  (** [('hd * 'tl) t] is a heterogeneous list containing a head of type ['hd T.t] and a
      tail of type ['tl t]. *)
  type _ t =
    | [] : unit t
    | ( :: ) : 'a T.t * 'b t -> ('a * 'b) t
end

module Id : sig
  type 'a t = 'a
end

(** [('hd * 'tl) t] is a heterogeneous list containing a head of type ['hd] and a tail of
    type ['tl t]. *)
type 'a t = 'a Gen(Id).t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a * 'b) t
