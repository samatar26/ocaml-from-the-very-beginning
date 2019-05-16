type colour = Red | Green | Blue | Yellow 

(* 
  The name of our new type is colour. 
  It has 4 constructors, which are the possible forms a value of type colour may take. 
 *)

let col : colour = Blue 

let cols : colour list = [Red; Red; Green; Yellow]

let colpair : char * colour = ('R', Red)

type colour = 
  | Red 
  | Green 
  | Blue 
  | Yellow 
  | RGB of int * int * int 

(* 
  We use _of_ in our RGB constructor to pass along extra information. 
 *)

let cols : colour list = [Red; Red; Green; Yellow; RGB (150, 0, 255)]


let components (c: colour) : (int * int * int) = 
  match c with 
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Yellow -> (255, 255, 0)
  | Blue -> (0, 0, 255)
  | RGB (r, g, b) -> (r, g, b)


(* 
  Types may contain a type variable like 'a, to allow the the (type of) part of the new type to be polymorphic, i.e. vary. 
  There's a built in option type: 

  type 'a option = None | Some of 'a - this can be read as:
  - a value of type 'a option is either nothing or something of type 'a.
 *)


let nothing = None

let number = Some 50 

let numbers = [Some 12; None; None; Some 2]

let word = Some ['c';'a';'k';'e']

(* 
  Note - The option type is useful as a more manageable alternative to exceptions where the lack of an anser is a common occurence. 
  I.e. looking up a value in a dictionary, we can return None instead of raising an exception. 
 *)

let rec lookup_opt x l = 
  match l with 
    [] -> None 
  | (k, v)::t -> if k = x then Some v else lookup_opt x t 


(* 
  Other than being polymorphic, new types may also be recursively defined, i.e. defined in terms of itself, using the type name in the body.
  This allows us to define our own lists. 
 *)


(* 
  Example recursive variant type where we define our own list.
 *)

type 'a sequence = Nil | Cons of 'a * 'a sequence

(*
  Nil is equivalent to []
  Cons is equivalent to the :: operator. 
  Cons carries two piece of data with it - one of type 'a (the head) and one of the 'a sequence (the tail) (which is the recursive part of our definition)

  [1] is the same as Cons (1, Nil) - int sequence 
  ['a';'x';'e'] is the same as Cons ('a', Cons ('x', Cons ( 'e', Nil)))

  Getting the last element in a list is harder than getting the first, because it's deeper in structure.
 *)

let rec length (l: 'a list) : int = 
  match l with 
  | [] -> 0 
  | _::t -> 1 + length t 

let rec append (a: 'a list) (b: 'a list) : 'a list = 
  match a with 
  | [] -> b 
  | h::t -> h :: append t b 



let rec length (l: 'a sequence) : int = 
  match l with 
  | Nil -> 0 
  | Cons (_, t) -> 1 + length t 

let rec append (a: 'a sequence) (b: 'a sequence) : 'a sequence = 
  match a with 
  | Nil -> b 
  | Cons (h, t) -> Cons (h, append t b)

(* 
  A type for mathematical expressions 
 *)

type expr = 
  | Num of int 
  | Add of expr * expr 
  | Subtract of expr * expr 
  | Multiply of expr * expr 
  | Divide of expr * expr 

 (*
  1 + 2 * 3 
  *)

let seven = Add (Num 1, Multiply (Num 2, Num 3))

let rec evaluate e = 
  match e with 
  | Num x -> x 
  | Add (e, e') -> evaluate e + evaluate e' 
  | Subtract (e, e') -> evaluate e + evaluate e' 
  | Multiply (e, e') -> evaluate e + evaluate e' 
  | Divide (e, e') -> evaluate e + evaluate e'
