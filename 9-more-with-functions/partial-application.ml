let add x y = x + y 
let f = add 6 

let rec map f l = 
  match l with 
    [] -> []
  | h::t -> f h :: map f t 

let map_add6 = map (add 6) [10; 20; 30]

let map_times2 = map (( * ) 2) [10;20;30]

(* 
  Note - Don't forget the space between the parentheses:
    ( * )
 *)

let rec mapl f l = 
  match l with 
  | [] -> []
  | h::t -> map f h :: mapl f t 

let mapl (f: ('a -> 'b)) (l: 'a list list) : 'b list list = map (map f) l 

let mapl (f: ('a -> 'b)) : ('a list list -> 'b list list) = map (map f) 

(* 
  This one was a bit of a mindbender, but got it in the end. 
  When we partially apply map to our function f, we get back a function that requires a list
  and will call the map function with that list.
  Since we've got a list of lists, our function f will be called everytime on our list of lists!

  Evaluation:

  map (map fun x -> x * 2) [[1;2;3;]]

  map (l -> calls map) [[1;2;3;]]

 *)

let add = fun x -> fun y -> x + y 
(* 
  shorthand is let add x y = x + y
 *)

