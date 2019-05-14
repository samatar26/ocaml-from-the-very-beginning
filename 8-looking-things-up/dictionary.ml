(* 
  To make a pair in OCaml, you just write it with parentheses and a comma in between the pair:
 *)

(* 
  Pairs are just a particular instance of a more general construct called the tuple. 
  A tuple may contain two or more things. 
 *)

let p : int * int = (1, 4)

(* 
  The pair does not need to have the same type:
 *)

let q : int * char = (1, '1')

let fst p = match p with (x, _) -> x 
let snd p = match p with (_, y) -> y 

(* 
  Since pairs only take one form, unlike lists which have two forms, namely
  empty or consisting of a head and a tail, OCaml lets use use this pattern directly in place of the argument: 
 *)

let fst (x, _) = x 
let snd (_, y) = y  

let census: (int * int) list = [(1,4); (2,2); (3,2); (4,3); (5,1); (6,2)]


let rec lookup x l = 
  match l with 
    [] -> raise Not_found 
  | (k, v)::t -> 
    if k = x then v 
    else lookup x t

let rec add k v d = 
  match d with 
    [] -> [(k, v)]
  | (k', v')::t -> 
    if k = k' 
    then (k,v)::t 
    else (k', v') :: add k v t 

let rec remove k d = 
  match d with 
    [] -> []
  | (k', v')::t -> 
    if k = k' 
    then t 
    else (k', v') :: remove k t

let rec key_exists k d = 
  try 
    let _ = lookup k d in true 
  with 
    Not_found -> false
