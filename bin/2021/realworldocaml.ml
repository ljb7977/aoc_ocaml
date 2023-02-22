open Base
open Stdio

let downcase_extension filename = 
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext

type point2d = {x:float ; y:float}

let magnitude {x; y} =
  Float.sqrt (x **. 2. +. y **. 2.)

let distance v1 v2 =
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y}

type circle_desc  = { center: point2d; radius: float }
type rect_desc    = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d }

type scene_element =
  | Circle of circle_desc
  | Rect   of rect_desc
  | Segment of segment_desc

let is_inside_scene_element point scene_element = 
  let open Float.O in
  match scene_element with
  | Circle { center; radius } -> distance center point < radius
  | Rect { lower_left; width; height } -> 
    point.x    > lower_left.x && point.x < lower_left.x + width
    && point.y > lower_left.y && point.y < lower_left.y + height
  | Segment _ -> false

let is_inside_scene point scene =
  List.exists scene
    ~f: (fun el -> is_inside_scene_element point el)

let rec read_and_accumulate accum = 
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
