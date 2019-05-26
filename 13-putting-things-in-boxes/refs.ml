let x = ref 0 

(* 
  x is a reference of type int ref which currently has contents 0.
  We can extract the current contents using the ! operator, which has type 'a ref -> 'a
 *)

let p = !x
(* p is equal to 0 *)

let _ = x := 50

let q = !x 
(* q now equals 50  and p is unchanged*)


let swap (a: 'a ref) (b: 'a ref): unit = 
  let temp_a = !a in 
  a := !b; 
  b := temp_a
(*  

When writing imperative code inside if..then..else, you have to surround the inner imperative expressions with parentheses,
so that the meaning isn't ambiguous. 

if x = y then 
  (a := !a + 1; 
  b:=!b -1)
else 
  c := !c + 1 

You can use begin and end instead for readibility: 
if x = y then 
begin
  a := !a + 1; 
  b:=!b -1
end
else 
  c := !c + 1 

*)

(*  for loop type unit *)

let print_5times () = 
  for x = 1 to 5 do 
    print_int x;
    print_newline ()
  done 

let smallest_pow2 (x: int) : int = 
  let t = ref 1 in 
  while !t < x do 
    t := !t * 2 
  done;
  !t

let print_histogram arr = 
  print_endline "Character frequencies:";
  for x = 0 to 255 do 
    if (arr.(x) > 0) then 
      begin 
        print_string "For character: ";
        print_char (char_of_int x); 
        print_string "'(character number "; 
        print_int x; 
        print_string ") the count is "; 
        print_int arr.(x); 
        print_newline ()
      end 
  done


let channel_statistics in_channel = 
  let lines = ref 0 in 
  let characters = ref 0  in 
  let words = ref 0 in 
  let sentences = ref 0  in 
  let histogram = Array.make 256 0 in
  try 
    while true do 
      let line = input_line in_channel in 
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter 
        (fun c -> 
           match c with 
             '.'|'?'|'!' -> sentences := !sentences + 1
           | ' '  -> words := !words + 1
           | _ -> ()
        )
        line;
      String.iter 
        (fun c -> 
           let i = int_of_char c in 
           histogram.(i) <- histogram.(i) + 1 )
        line
    done 
  with 
    End_of_file -> 
    print_string "There were ";
    print_int !lines;
    print_string " lines, making up ";
    print_int !characters;
    print_string " characters with ";
    print_int !words;
    print_string " words in ";
    print_int !sentences;
    print_string " sentences.";
    print_newline ();
    print_histogram histogram


let file_statistics filename = 
  let channel = open_in filename 
  in 
  try 
    channel_statistics channel;
    close_in channel 
  with 
    _ -> close_in channel 


(* Arrays *)

(* 
  An array in OCaml is a place for storing a fixed number of elements of like type. 
 *)

let a = [|1; 2; 3; 4; 5|]

(* Accessing an element inside our array is done in constant time by giving the position of the element (known as the subscript) in parentheses: *)

let one = a.(0)

(* We can update any of the values in the array in constant time, like so  *)

let _ = a.(0) <- 100 

(* Finding th  length of an array is constant, since the array's length was fixed when the array was created. *)

let five = Array.length a 

let new_a = Array.make 10 true 

(* 
  So, if we were to store the count for each character in a string an array would come in pretty useful.
  Especially if we store the characters via their ASCII code and using int_of_char and char_of_int!
 *)


