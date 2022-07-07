open Aoc_ocaml

let input_file = "resource/2021/day3.input"

exception E of int
exception SameFrequencyException

let find_most_common_digit digits =
  let result =
    List.fold_left
      (fun counter digit ->
        let zero, one = counter in
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
  let nths =
    List.fold_left (fun acc l -> List.nth l n :: acc) [] list_of_list
  in
  List.rev nths

let transpose list_of_list =
  let length = List.length (List.hd list_of_list) in
  let range = List.init length (fun x -> x) in
  List.map (fun idx -> get_nth_from_lists list_of_list idx) range

let parse_line str =
  let ints =
    String.fold_left
      (fun acc c -> (int_of_char c - int_of_char '0') :: acc)
      [] str
  in
  List.rev ints

let negate list_of_ints =
  List.map
    (fun i -> match i with 0 -> 1 | 1 -> 0 | x -> raise (E x))
    list_of_ints

let binary_to_decimal list_of_int =
  let rec inner list_of_int base value =
    match list_of_int with
    | [] -> value
    | first :: rest -> inner rest (base * 2) (value + (first * base))
  in
  inner (List.rev list_of_int) 1 0

let () =
  let lines = read_lines input_file in
  let parsed_lines = List.map parse_line lines in
  (* List.iter (fun t -> print_int t) (List.hd parsed_lines) *)
  let transposed = transpose parsed_lines in
  let most_common_digits = List.map find_most_common_digit transposed in
  let least_common_digits = negate most_common_digits in
  let gamma = binary_to_decimal most_common_digits in
  let epsilon = binary_to_decimal least_common_digits in
  print_int (gamma * epsilon);
  print_newline ()
