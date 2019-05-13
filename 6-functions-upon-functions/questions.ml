(* 
Questions
1. Write a simple recursive function calm to replace exclamation marks in a char list with periods. For
example calm ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'] should evaluate to
calm ['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.'] . Now rewrite your function
to use map instead of recursion. What are the types of your functions?
2. Write a function clip which, given an integer, clips it to the range 1 . . . 10 so that integers bigger
than 10 round down to 10, and those smaller than 1 round up to 1. Write another function cliplist
which uses this first function together with map to apply this clipping to a whole list of integers.
3. Express your function cliplist again, this time using an anonymous function instead of clip .
4. Write a function apply which, given another function, a number of times to apply it, and an initial
argument for the function, will return the cumulative effect of repeatedly applying the function. For
instance, apply f 6 4 should return f (f (f (f (f (f 4)))))) . What is the type of your function?
5. Modify the insertion sort function from the preceding chapter to take a comparison function, in the
same way that we modified merge sort in this chapter. What is its type?
6. Write a function filter which takes a function of type α → bool and an α list and returns a list of
just those elements of the argument list for which the given function returns true .
7. Write the function for_all which, given a function of type α → bool and an argument list of type
α list evaluates to true if and only if the function returns true for every element of the list. Give
examples of its use.
8. Write a function mapl which maps a function of type α → β over a list of type α list list to produce
a list of type β list list.
 *)

let rec map f l = 
  match l with 
    [] -> [] 
  | h::t -> f h :: map f t

let rec calm (l: char list): char list = 
  match l with 
    [] -> [] 
  | '!'::t -> '.' :: calm t 
  | h::t -> h :: calm t

let calm_char x = 
  match x with 
    '!' -> '.'
  |  _ -> x 

let calm_v2 l = 
  map calm_char l

let clip (x:int) : int = 
  if x < 1 then 1 else
  if x > 10 then 10 else x 

let cliplist (l: int list): int list = 
  map clip l 

let cliplist_v2 (l: int list): int list = 
  map 
    (fun x -> 
       if x < 1 then 1 else 
       if x > 10 then 10 else x)
    l



let rec apply (f: ('a -> 'a)) (n: int) (x: 'a): 'a = 
  match n with 
    0 -> x 
  |  _ -> apply f (n-1) (f x)

(* 
  vs. f (apply f (n-1) x)
  I believe my solution is tail recursive, whereas the book one isn't. 
 *)


let rec sort (cmp: ('a -> 'a -> bool)) (l: 'a list): 'a list = 
  let rec insert x s = 
    match s with 
      [] -> [x]
    | h::t -> 
      if cmp x h  
      then x :: h :: t 
      else h :: insert x t 
  in 
  match l with 
    [] -> []
  |  h::t -> insert h (sort cmp t)


let rec filter (f: 'a -> bool) (l: 'a list): 'a list = 
  match l with 
    [] -> []
  | h::t -> 
    if f h 
    then h :: filter f t 
    else filter f t

let rec for_all (f: 'a -> bool) (l: 'a list) = 
  match l with 
    [] -> true
  | h::t -> f h && for_all f t 

let rec mapl (f: 'a -> 'b) (l: 'a list list): 'b list list = 
  match l with 
    [] -> []
  | h::t -> map f h :: mapl f t

(* 
  recursion to handle outer list and map to handle inner list.
 *)