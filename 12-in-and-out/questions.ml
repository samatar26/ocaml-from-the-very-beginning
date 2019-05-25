(* 
  Questions
1. Write a function to print a list of integers to the screen in the same format OCaml uses – i.e. with
square brackets and semicolons.
2. Write a function to read three integers from the user, and return them as a tuple. What exceptions
could be raised in the process? Handle them appropriately.
3. In our read_dict function, we waited for the user to type 0 to indicate no more data. This is
clumsy. Implement a new read_dict function with a nicer system. Be careful to deal with possible
exceptions which may be raised.
4. Write a function which, given a number x, prints the x-times table to a given file name. For example,
table "table.txt" 5 should produce a file table.txt containing the following:
1 2 3 4 5
2 4 6 8 10
3 6 9 12 15
4 8 12 16 20
5 10 15 20 25
Adding the special tabulation character '\t' after each number will line up the columns.
5. Write a function to count the number of lines in a given file.
6. Write a function copy_file of type string → string → unit which copies a file line by line. For
example, copy_file "a.txt" "b.txt" should produce a file b.txt identical to a.txt . Make sure
you deal with the case where the file a.txt cannot be found, or where b.txt cannot be created or
filled.
 *)



let rec print_int_list (l: int list) : unit =
  print_string "[";
  let rec print_ints = function 
    | [] -> ()
    | [i] -> print_int i 
    | h::t -> print_string ((string_of_int h) ^ "; "); print_ints t 
  in print_ints l;
  print_string "]" 


let rec read_int_to_tuple () = 
  try 
    print_endline "Type three integers, pressing Enter after each.";
    let x = read_int () 
    in let y = read_int()
    in let z = read_int () 
    in (x, y, z)
  with 
    Failure "int_of_string" ->
    print_endline "Failed to read integers. Please try again."; 
    read_int_to_tuple()

(* 
  When doing     (read_int () , read_int () , read_int ())
  the evaluation order isn't specified.
 *)

let rec read_dict n = 
  if n = 0 then [] else 
    try 
      let i = read_int() in 
      let name =read_line () 
      in (i, name) :: read_dict (n-1)
    with 
      Failure "int_of_string" -> 
      print_endline "This is not a valid integer.";
      print_endline " Please enter integer and name again.";
      read_dict n

exception BadNumber 


let rec read_dict () = 
  print_endline "How many dictionary entries to input";
  try 
    let n = read_int() in 
    if n < 0 then raise BadNumber else read_dict ()
  with 
    Failure "int_of_string" -> 
    print_endline "Not a number. Try again"; 
    read_dict ()
  | BadNumber -> 
    print_endline "The number is negative, please enter a positive integer.";
    read_dict ()


let table filename n = 
  let ch = open_out filename 
  in let rec write_table row column = 
       match row, column with 
       | x, y -> 
         if x = n && y = n then 
           (output_string ch (string_of_int (x*y));
            close_out ch )
         else if y = n then 
           (output_string ch (string_of_int (x * y) ^ "\n");
            write_table (x + 1) 1)
         else 
           (output_string ch (string_of_int (x * y) ^ "\t");
            write_table x (y + 1))
  in write_table 1 1



(* book solution *)

let rec iter f l = 
  match l with 
  |  [] -> () 
  | h::t -> f h; iter f t

let rec map f l = 
  match l with 
  | [] -> []
  | h::t -> f h :: map f t 

let rec numlist n = 
  match n with 
    0 -> [] 
  | _ -> numlist (n -1 ) @ [n]

let write_row ch x one_to_n = iter (fun i -> 
    output_string ch (string_of_int i);
    output_string ch "\t")
    (map (( * ) x) (one_to_n))

let write_table_channel ch n =
  let one_to_n = numlist n in 
  iter
    (fun x ->
       write_row ch x one_to_n; 
       output_string ch "\n")
    (one_to_n)


(* 
  E.g. if I pass in x = 1 and n = 10 
  map (( * ) x) (numlist n)) will produce a list of 1 * 1, 1 *2 ... 1 * 10.
 *)


exception FileProblem 

let table filename n = 
  if n < 0 then raise (Invalid_argument "table") else 
    try 
      let ch = open_out filename in 
      write_table_channel ch n; 
      close_out ch 
    with 
      _ -> raise FileProblem 

(* 5. *)

let rec count_lines ch acc = 
  try
    let _ = input_line ch in 
    count_lines ch (acc + 1)
  with  
    End_of_file -> acc

let countlines file = 
  try 
    let ch = open_in file in 
    let result = count_lines ch 1 in 
    close_in ch;
    result 
  with 
    _ -> raise (Failure "countlines")


let copy_file inputfile outputfile = 
  let output_channel = open_out outputfile 
  in let input_channel = open_in inputfile 
  in  let rec write_to_output_channel () = 
        try 
          let line =  input_line input_channel ^ "\n"
          in let _ = output_string output_channel line
          in write_to_output_channel ()
        with 
          End_of_file -> 
          close_in input_channel;
          close_out output_channel
  in write_to_output_channel()

let rec copy_file_ch from_ch to_ch = 
  try 
    let line =  input_line from_ch ^ "\n"
    in let _ = output_string to_ch line
    in copy_file_ch from_ch to_ch
  with 
    End_of_file -> ()

exception CopyFailed

let copy_file from_name to_name = 
  try let output_channel = open_out to_name in
    let input_channel = open_in from_name in 
    copy_file_ch input_channel output_channel;
    close_in input_channel; 
    close_out output_channel
  with 
    _ -> raise CopyFailed 
