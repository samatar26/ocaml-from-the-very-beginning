(* 
  Questions
1. In msort , we calculate the value of the expression length l / 2 twice. Modify msort to remove
this inefficiency.
2. We know that take and drop can fail if called with incorrect arguments. Show that this is never the
case in msort .
3. Write a version of insertion sort which sorts the argument list into reverse order.
4. Write a function to detect if a list is already in sorted order.
5. We mentioned that the comparison functions like < work for many OCaml types. Can you
determine, by experimentation, how they work for lists? For example, what is the result of [1; 2]
< [2; 3] ? What happens when we sort the following list of type char list list? Why?
[['o'; 'n'; 'e']; ['t'; 'w'; 'o']; ['t'; 'h'; 'r'; 'e'; 'e']]
6. Combine the sort and insert functions into a single sort function.
 *)

let rec take n l = 
  if n = 0 then []
  else match l with 
      [] -> []
    | h::t -> h :: take (n-1) t

let rec drop n l = 
  if n = 0 then l 
  else match l with 
      [] -> l
    | h::t -> drop (n-1) t

let rec length l = 
  match l with 
    [] -> 0 
  | h::t -> 1 + length t

let rec merge x y = 
  match x, y with 
    [], l -> l 
  | l, [] -> l 
  | hx::tx, hy::ty -> 
    if hx < hy 
    then hx :: merge tx (hy::ty)
    else hy :: merge ty (hx::tx)

let rec msort l = 
  match l with 
    [] -> []
  | [x] -> [x]
  | _ -> 
    let x = length l / 2 in 
    let left = take x l in
    let right =  drop x l in
    merge (msort left) (msort right)

(* 
2. The argument to take and drop is length l / 2, which is always less than or equal to 
  length l for all possible values of l. The static typing guarantees that we always pass a list in too,
  so no chance of user error as the length of the list will always be >= 0. 
 *)

let rec insert x l = 
  match l with 
    [] -> [x]
  | h::t -> 
    if x >= h 
    then x :: h :: t 
    else h :: insert x t

let rec sort l = 
  match l with 
    [] -> []
  | h::t -> insert h (sort t)

let rec is_sorted l = 
  match l with 
    a::b::t -> a <= b && is_sorted (b::t) 
  | _ -> true

(* 
  5. Lists are compared using lexographical order, i.e. first the first elements are considered
    if they're the same then the second elements, etc.  
 *)

let rec insertion_sort l = 
  let rec insert x s = 
    match s with 
      [] -> [x]
    | h::t -> 
      if x <= h 
      then x :: h :: t 
      else h :: insert x t 
  in match l with 
    [] -> []
  | h::t -> insert h (sort t)