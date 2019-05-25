let print_dict_entry ((k, v): int * string) : unit = 
  print_int k; 
  print_newline (); 
  print_string v;
  print_newline ()

let rec print_dict = function 
  | [] -> ()
  | h::t -> print_dict_entry h; print_dict t

let rec iter (f: 'a -> 'b) = function
  | [] -> ()
  | h::t -> f h; iter f t 

let print_dict d = iter print_dict_entry d

let print_dict = iter print_dict_entry

let rec read_dict () =
  try 
    let i = read_int () in 
    if i = 0 then [] else 
      let name = read_line () in 
      (i, name) :: read_dict ()
  with 
    Failure "int_of_string" -> 
    print_endline "This is not a valid integer, please try again."; 
    read_dict ()

(* 
  The read_int and read_line wait for a user's input and pressing the enter key.
 *)


(* 
  When reading/writing from files, OCaml has some basic functions we can use.
  Places we read from have type in_channel and places we can write to have type out_channel.
  *)


let entry_to_channel (ch: out_channel) ((k, v): int * string): unit = 
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let dictionary_to_channel ch d = 
  iter (entry_to_channel ch) d

(* 
  We can create an output channel by using the function open_out,
  which gives us an output channel for the filename given as a string. 

  Once we've written the contents to the file, we must call close_out to properly close the file. 
 *)


let dictionary_to_file filename dict =
  let ch = open_out filename in 
  dictionary_to_channel ch dict;
  close_out ch 


(* 
  We can now read the file back in:
 *)

let entry_of_channel ch = 
  let number = input_line ch in 
  let name = input_line ch in 
  (int_of_string number, name)

let rec dictionary_of_channel ch = 
  try 
    let e = entry_of_channel ch in 
    e :: dictionary_of_channel ch 
  with 
    End_of_file -> []

let dictionary_of_file filename = 
  let ch = open_in filename in 
  let dict = dictionary_of_channel ch 
  in close_in ch; 
  dict