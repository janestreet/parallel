open! Base
module In = Stdlib.In_channel
module Out = Stdlib.Out_channel
module Scanf = Stdlib.Scanf.Scanning

type t =
  { width : int
  ; height : int
  ; data : float array
  }

let save { width; height; data } file =
  Out.with_open_text file (fun out ->
    let header = Stdlib.Printf.sprintf "P2\n%d %d\n255\n" width height in
    let data =
      Array.(fold_right [@mode shared]) data ~init:[] ~f:(fun x acc ->
        (x
         |> Float.clamp_exn ~min:0.0 ~max:1.0
         |> Float.( * ) 255.0
         |> Float.round_nearest
         |> Float.to_int
         |> Int.to_string)
        :: acc)
      |> List.chunks_of ~length:width
      |> List.map ~f:(String.concat ~sep:" ")
      |> String.concat ~sep:"\n"
    in
    Out.output_string out header;
    Out.output_string out data)
;;

let load file =
  In.with_open_text file (fun in_ ->
    let in_ = Scanf.from_channel in_ in
    let width, height, max =
      Stdlib.Scanf.bscanf in_ "P2\n%d %d\n%d\n" (fun width height max ->
        width, height, max)
    in
    let max = Float.of_int max in
    let data =
      Array.init (width * height) ~f:(fun _ ->
        Stdlib.Scanf.bscanf in_ " %f " (fun f -> f /. max))
    in
    Scanf.close_in in_;
    { width; height; data })
;;

let of_array data ~width ~height =
  if Array.length data <> width * height then invalid_arg "mismatched array size";
  { width; height; data }
;;

let width { width; _ } = width
let height { height; _ } = height

let get { data; width; _ } ~x ~y =
  let idx = x + (y * width) in
  (Array.get [@mode shared]) data idx
;;

let set { data; width; _ } ~x ~y p =
  let idx = x + (y * width) in
  Array.set data idx p
;;
