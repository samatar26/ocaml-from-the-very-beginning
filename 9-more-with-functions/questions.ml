(* 
  Questions
1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c .
2. Recall the function member x l which determines if an element x is contained in a list l . What is its
type? What is the type of member x ? Use partial application to write a function member_all x ls
which determines if an element is a member of all the lists in the list of lists ls .
3. Why can we not write a function to halve all the elements of a list like this: map (( / ) 2) [10; 20;
30] ? Write a suitable division function which can be partially applied in the manner we require.
4. Write a function mapll which maps a function over lists of lists of lists. You must not use the let
rec construct. Is it possible to write a function which works like map , mapl , or mapll depending
upon the list given to it?
5. Write a function truncate which takes an integer and a list of lists, and returns a list of lists, each
of which has been truncated to the given length. If a list is shorter than the given length, it is
unchanged. Make use of partial application.
6. Write a function which takes a list of lists of integers and returns the list composed of all the first
elements of the lists. If a list is empty, a given number should be used in place of its first element.
 *)

(* 
1. 
The function g a b c has type α → β → γ -> 𝛿 which can also be written α → (β → (γ -> 𝛿)). Thus, it takes an 
argument of type α  and return a function of type β → (γ -> 𝛿) which when you give it an argument of type β returns a function 
of type γ -> 𝛿, which when you give it an argument of type γ returns something of type 𝛿.  And so, we can apply just one argument to the function g (which is called partial application), or apply all three at once. 
When we write let g a b c = ... this is just shorthnd for let g = fun a -> fun b -> fun c -> ...    
 *)

let rec map f l =   
  match l with 
  | [] -> []
  | h::t -> f h :: map f t

let rec member (x: 'a) l : bool = 
  match l with 
  | [] -> false 
  | h::t -> h = x || member x t 

let member_all (x: 'a) (ls: 'a list list): bool = 
  let booleans = map (member x) ls in 
  not (member false booleans)


let divide x y = y / x

let mapll f (l: 'a list list list) : 'b list list list = map (map (map f)) l 

let mapll f = map (map (map f))

(* 
  Not possible to write a function which would map function f over a list, a lists of lists or a lists of lists of lists, etc. 
  This is because in OCaml every function must have a single type. 
 *)

let rec take n l = 
  match l with 
  | [] ->  
    if n = 0 then []
    else raise (Invalid_argument "take") 
  | h::t -> 
    if n < 0 then raise (Invalid_argument "take")
    else if n = 0 then []
    else h :: take (n -1) t

let rec length l = 
  match l with 
  | [] -> 0 
  | h::t -> 1 + length t


let truncate_l n l = 
  if length l >= n then take n l else l 
(* try take n l with Invalid_argument "take" -> l *)

let truncate n ll = 
  map (truncate_l n) ll

(* Literally calling truncate function on each list of lists :mind_blown *)


let first_l n l = try List.hd l with Failure "hd" -> n
(* match l with [] -> n | h::_ -> h *)

let first_ll n l = map (first_l n) l


