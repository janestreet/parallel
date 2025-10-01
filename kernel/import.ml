module Atomic = Portable.Atomic
module Capsule = Portable.Capsule.Expert

(* Shadow [Base.Result]. *)
module Result = Result

(* Shadow [Base.Effect] which is a [[@deprecated]] alias for [Stdlib.Effect]. *)
module Effect = Effect
