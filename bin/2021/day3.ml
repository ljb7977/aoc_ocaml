open Aoc_ocaml

let input_file = "resource/2021/day3.input"
let print_list_of_int list_of_int = List.iter (fun i -> print_int i) list_of_int

exception E of int

let find_most_common_digit digits =
  let num_zero, num_one =
    List.fold_left
      (fun counter digit ->
        let num_zero, num_one = counter in
        match digit with
        | 0 -> (num_zero + 1, num_one)
        | 1 -> (num_zero, num_one + 1)
        | x -> raise (E x))
      (0, 0) digits
  in
  if num_zero > num_one then 0 else 1

let find_least_common_digit digits =
  let most_common_digit = find_most_common_digit digits in
  match most_common_digit with 0 -> 1 | 1 -> 0 | x -> raise (E x)

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

let complement list_of_ints =
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

(* Part 1 *)
let () =
  let lines = read_lines input_file in
  let parsed_lines = List.map parse_line lines in
  let transposed = transpose parsed_lines in
  let most_common_digits = List.map find_most_common_digit transposed in
  let least_common_digits = complement most_common_digits in
  let gamma = binary_to_decimal most_common_digits in
  let epsilon = binary_to_decimal least_common_digits in
  print_int (gamma * epsilon);
  print_newline ()

let range v = List.init v (fun i -> i)

let get_rating (numbers : int list list) rate_function =
  let inner (numbers : int list list) (target_index : int) =
    if List.length numbers == 1 then numbers
    else
      let digits_at_i =
        List.map (fun number -> List.nth number target_index) numbers
      in
      let target_digit = rate_function digits_at_i in
      List.filter
        (fun number -> List.nth number target_index == target_digit)
        numbers
  in
  let idxs = range (List.length (List.hd numbers)) in
  let reduced = List.fold_left (fun acc idx -> inner acc idx) numbers idxs in
  List.hd reduced

(* Part 2 *)
let () =
  let lines = read_lines input_file in
  let parsed_lines = List.map parse_line lines in
  (* List.iter (fun line -> print_list_of_int line; print_newline()) parsed_lines *)
  let oxygen_rating = get_rating parsed_lines find_most_common_digit in
  let oxygen_rating = binary_to_decimal oxygen_rating in
  let co2_rating = get_rating parsed_lines find_least_common_digit in
  let co2_rating = binary_to_decimal co2_rating in
  print_int (oxygen_rating * co2_rating);
  print_newline ()
