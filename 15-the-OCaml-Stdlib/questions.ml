(* 
  Questions
1. Write your own version of the function List.concat . The implementation OCaml provides is not
tail-recursive. Can you write one which is?
2. Use List.mem to write a function which returns true only if every list in a bool list list contains
true somewhere in it.
3. Write a function to count the number of exclamation marks in a string, using one or more functions
from the String module.
4. Use the String.map function to write a function to return a new copy of a string with all exclamation
marks replaced with periods (full stops).
5. Use the String module to write a function which concatenates a list of strings together.
6. Do the same with the Buffer module. This will be faster.
7. Use the String module to count the number of occurrences of the string "OCaml" within a given
string.
 *)

let concat (l: 'a list list) =
  let rec concat_inner acc s = 
    match s with 
    | [] -> List.rev acc
    | h::t -> concat_inner (List.rev h @ acc) t 
  in concat_inner [] l

(* 
    With the @ operator it's runtime complexity is equal to the length of the argument on the left. 
    So it's better to do h @ acc rather than acc @ h everytime!
   *)

let rec all_contains_true l = 
  match l with 
  | [] -> true
  | h::t -> List.mem true h && all_contains_true t

let all_contain_true l = 
  not (List.mem false (List.map (List.mem true) l))

let count_exclamations s = 
  let n = ref 0 in 
  String.iter (function '!' -> n := !n +1 | _ -> ()) s;
  n


let calm  = 
  String.map (function '!' -> '.' | x -> x) 

let concat_string = 
  String.concat "" 


let concat ls = 
  let b = Buffer.create 100 in 
  List.iter (Buffer.add_string b) ls; 
  Buffer.contents b

let ocaml ss s = 
  if String.length s >= String.length ss then 
    let count = ref 0 in
    for x = 0 to String.length s - String.length ss  do 
      if String.sub s x (String.length ss) = ss then  
        count := !count + 1;
    done; 
    !count
  else 0 


(* book solution *)
let occurrences ss s =
  if ss = "" then 0 else
    let num = ref 0 in
    let str = ref s in
    while
      String.length ss <= String.length !str && !str <> ""
    do
      if String.sub !str 0 (String.length ss) = ss then
        num := !num + 1;
      str := String.sub !str 1 (String.length !str - 1)
    done;
    !num