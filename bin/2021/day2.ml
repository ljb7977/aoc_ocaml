open Aoc_ocaml
let file = "resource/2021/day2.input"

type command = Forward of int | Up of int | Down of int
type coord = { x : int; y : int }

exception E of string

let parse_line l =
  let strings = String.split_on_char ' ' l in
  match strings with
  | cmd :: amount :: _ -> (
      let amount = int_of_string amount in
      match cmd with
      | "forward" -> Forward amount
      | "up" -> Up amount
      | "down" -> Down amount
      | _ -> raise (E "Unknown command"))
  | _ -> raise (E "Command format is wrong")

(* Part 1 *)
let apply_command coord command =
  let { x; y } = coord in
  match command with
  | Forward a -> { x = x + a; y }
  | Up a -> { x; y = y + a }
  | Down a -> { x; y = y - a }

let () =
  let lines = read_lines file in
  let commands = List.map parse_line lines in
  let final_coord = List.fold_left apply_command { x = 0; y = 0 } commands in
  let { x; y } = final_coord in
  let product = x * y in
  let t = abs product in
  print_int t;
  print_newline ()

(* Part 2 *)
type aim_and_coord = { x : int; y : int; aim : int }

let apply_command_2 { aim; x; y } = function
  | Down a -> { x; y; aim = aim + a }
  | Up a -> { x; y; aim = aim - a }
  | Forward a -> { x = x + a; y = y + (aim * a); aim }

let () =
  let lines = read_lines file in
  let commands = List.map parse_line lines in
  let final_coord =
    List.fold_left apply_command_2 { x = 0; y = 0; aim = 0 } commands
  in
  let { x; y; _ } = final_coord in
  let product = x * y in
  let t = abs product in
  print_int t;
  print_newline ()
