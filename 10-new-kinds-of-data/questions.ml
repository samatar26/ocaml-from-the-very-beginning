(* 
  Questions
1. Design a new type rect for representing rectangles. Treat squares as a special case.
2. Now write a function of type rect → int to calculate the area of a given rect.
3. Write a function which rotates a rect such that it is at least as tall as it is wide.
4. Use this function to write one which, given a rect list, returns another such list which has the
smallest total width and whose members are sorted widest first.
5. Write take , drop , and map functions for the sequence type.
6. Extend the expr type and the evaluate function to allow raising a number to a power.
7. Use the option type to deal with the problem that Division_by_zero may be raised from the
evaluate function.
 *)

type rect = 
  | Rectangle of int * int 
  | Square of int

let area r =  
  match r with 
  | Rectangle (x, y) -> x * y
  | Square (x) -> x * x


let rotate r = 
  match r with 
  | Rectangle (x, y) -> if x > y then Rectangle (y, x) else r
  | Square _ -> r 

let rec map f l = 
  match l with 
  | [] -> []
  | h::t -> f h :: map f t 

let rec sort cmp l = 
  let rec insert x s = 
    match s with 
    |  [] -> [x]
    | h::t -> 
      if cmp x h 
      then x :: h :: t 
      else h:: insert x t 
  in match l with 
  | [] -> []
  | h::t -> insert h (sort cmp t)


let width_of_rect r = 
  match r with 
  | Square s -> s 
  | Rectangle (x, _) -> x 

let rect_compare a b = width_of_rect a < width_of_rect b

let pack rects = sort rect_compare (map rotate rects) 

type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec take n l = 
  if n = 0 then Nil else 
    match l with 
    | Nil -> raise (Invalid_argument "take")
    | Cons (h, t) -> Cons(h, (take (n -1) t))

let rec drop n l = 
  if n = 0 then l else 
    match l with 
    | Nil -> raise (Invalid_argument "drop")
    | Cons (_, t) -> drop (n -1) t

let rec map f l = 
  match l with 
  | Nil -> Nil  
  | Cons (h, t) -> Cons (f h, map f t)


type expr = 
  | Num of int 
  | Add of expr * expr 
  | Subtract of expr * expr 
  | Multiply of expr * expr 
  | Divide of expr * expr 
  | Power of expr * expr

let rec power x n = 
  if n = 0 then 1 else x * power x (n -1)

let rec evaluate e = 
  match e with 
  | Num x -> x 
  | Add (e, e') -> evaluate e + evaluate e' 
  | Subtract (e, e') -> evaluate e - evaluate e' 
  | Multiply (e, e') -> evaluate e * evaluate e' 
  | Divide (e, e') -> evaluate e / evaluate e'
  | Power (e, e') -> power (evaluate e) (evaluate e')

let evaluate_opt e = 
  try Some (evaluate e) with Division_by_zero -> None