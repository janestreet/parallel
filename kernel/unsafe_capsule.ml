open! Base
open! Import
open Capsule

external unsafe_rebrand : _ Access.t @ contended -> _ Access.t @@ portable = "%identity"

let dummy = Obj.magic_portable initial

let[@inline] access_local__promise_no_exn ~password:_ ~f = exclave_
  f (unsafe_rebrand dummy)
;;

module Data = struct
  external unsafe_unwrap : ('a, _) Data.t @ local -> 'a @ local @@ portable = "%identity"

  let[@inline] iter_local__promise_no_exn t ~password:_ ~f =
    f (unsafe_unwrap t) [@nontail]
  ;;

  let[@inline] extract__promise_no_exn t ~password:_ ~f = f (unsafe_unwrap t) [@nontail]
end
