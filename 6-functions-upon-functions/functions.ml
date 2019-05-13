let rec double l = 
  match l with 
    [] -> []
  | h::t -> (h * 2) :: double t

let rec evens l = 
  match l with 
    [] -> []
  | h::t -> (h mod 2 = 0) :: evens t

(* 
  Would be tedious if wanted to write a similar function each time we wanted to apply a different operation 
  to every element of a list. In comes map. 
 *)

let rec map f l = 
  match l with 
    [] -> []
  | h::t -> f h :: map f t

let halve x = x / 2

let is_even (x: int) : bool = x mod 2 = 0 

let evens_v2 (l: int list) : bool list = map is_even l

(* we can make evens even shorter by using an anonymous function *)

let evens_v3 (l: int list) : bool list = 
  map (fun x -> x mod 2 = 0) l

let greater a b = 
  a >= b 

let rec length l = 
  match l with 
    [] -> 0 
  | h::t -> 1 + length t 

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

let rec merge cmp x y = 
  match x, y with 
    [], l -> l 
  | l, [] -> l 
  | hx::tx, hy::ty -> 
    if cmp hx hy
    then hx :: merge cmp tx (hy::ty)
    else hy :: merge cmp (hx::tx) ty

let rec msort cmp l = 
  match l with 
    [] -> []
  | [x] -> [x]
  | h::t -> 
    let x = length l / 2
    in let left = take x l 
    in let right = drop x l 
    in merge cmp (msort cmp left) (msort cmp right)

(* 
We can turn an infix operator into a prefix one by enclosing it in parentheses.
merge (<=) [5;4;3;2;1] will work!!
 *)