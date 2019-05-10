(* 
Questions
1. Rewrite the not function from the previous chapter in pattern matching style.
2. Use pattern matching to write a recursive function which, given a positive integer n, returns the
sum of all the integers from 1 to n.
3. Use pattern matching to write a function which, given two numbers x and n, computes x n .
4. For each of the previous three questions, comment on whether you think it is easier to read the
function with or without pattern matching. How might you expect this to change if the functions
were much larger?
5. What does match 1 + 1 with 2 -> match 2 + 2 with 3 -> 4 | 4 -> 5 evaluate to?
  5.
6. There is a special pattern x..y to denote continuous ranges of characters, for example 'a'..'z'
will match all lowercase letters. Write functions islower and isupper , each of type char → bool, to
decide on the case of a given letter.
 *)


let not x = 
  match x with 
    true -> false
  | false -> true;;

let rec sum x = 
  match x with 
    1 -> 1
  | _ -> x + sum (x -1);;

let rec power x n = 
  match n with 
    0 -> 1 
  | _ -> x * power x (n -1);;


let isLower x = 
  match x with 
  'a' .. 'z' -> true 
  | _ -> false;;

let isUpper x = 
  match x with 
  'A' .. 'Z' -> true 
  | _ -> false;;

(* 
Can't really do not (isLower x) for the isUpper solution, 
because for erroneous arguments such as punctuation it'll still return true. 
Because all it's checking is if it's just not lowercase.
 *)