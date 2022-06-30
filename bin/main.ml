open Core.Std
let file = "resource/2021/day2.input"

type command = { cmd : string; amount : int }

let parse_line l =
  let strings = String.split_on_char ' ' l in
  match strings with
  | cmd :: amount :: _ -> Some { cmd; amount = int_of_string amount }
  | _ -> None

let () =
  let lines = In_channel.read_lines file in
  List.map parse_line lines

type coord = { x : int; y : int }

let c = { x = 0; y = 0 }
