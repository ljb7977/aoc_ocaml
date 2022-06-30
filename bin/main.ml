let file = "resource/2021/day2.input"

type command = Forward of int | Backward of int | Up of int | Down of int
type coord = { x : int; y : int }

let read_lines name : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

exception E of string

let parse_line l =
  let strings = String.split_on_char ' ' l in
  match strings with
  | cmd :: amount :: _ -> (
      let amount = int_of_string amount in
      match cmd with
      | "forward" -> Forward amount
      | "backward" -> Backward amount
      | "up" -> Up amount
      | "down" -> Down amount
      | _ -> raise (E "Unknown command"))
  | _ -> raise (E "Command format is wrong")

let apply_command acc cmd =
  let { x; y } = acc in
  match cmd with
  | Forward a -> { x = x + a; y }
  | Backward a -> { x = x - a; y }
  | Up a -> { x; y = y + a }
  | Down a -> { x; y = y - a }

let last =
  let lines = read_lines file in
  let commands = List.map parse_line lines in
  List.fold_left apply_command { x = 0; y = 0 } commands

let () =
  let { x; y } = last in
  let product = x * y in
  let t = abs product in
  print_int t;
  print_newline ()
