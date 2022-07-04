open Aoc_ocaml

let input_file = "resource/2021/day3.input"

(* type counter = int * int *)

exception E of int

exception SameFrequencyException

let find_most_common_digit digits =
  let result =
    List.fold_left
      (fun counter digit ->
        let (zero, one) = counter in
        match digit with
        | 0 -> (zero + 1, one)
        | 1 -> (zero, one + 1)
        | x -> raise (E x))
      (0, 0) digits
  in
  match result with
  | zero, one when zero > one -> 0
  | zero, one when one > zero -> 1
  | _ -> raise SameFrequencyException

let get_nth_from_lists list_of_list n =
  let nths = List.fold_left (fun acc l -> (List.nth l n) :: acc) [] list_of_list in
  List.rev nths

let transpose list_of_list = 
  let length = List.length (List.hd list_of_list) in
  let range = List.init length (fun x -> x) in
  List.map (fun idx -> get_nth_from_lists list_of_list idx) range

let parse_line str =
  let ints = String.fold_left (fun acc s -> int_of_char s :: acc) [] str in
  List.rev ints

let () =
  let lines = read_lines input_file in
  let parsed_lines = List.map parse_line lines in
  List.iter (fun t -> print_int t) (List.hd parsed_lines)
  (* let transposed = transpose parsed_lines in
  let most_common_digits = List.map find_most_common_digit transposed in
  List.iter (fun t -> print_int t) most_common_digits;
  print_newline(); *)
