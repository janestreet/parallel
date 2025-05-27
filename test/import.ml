module Capsule = Portable.Capsule.Expert

external runtime5 : unit -> bool = "%runtime5"

let is_runtime5 = runtime5 ()
