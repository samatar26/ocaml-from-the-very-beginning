let rec odd_elements l = 
  match l with 
    h::_::t -> h :: odd_elements t
  | _ -> l;;

let rec append a b = 
  match a with 
    [] -> b 
  | [h] -> h :: b
  | h::t -> append t (h::b);;

(* vs. book solution *)

let rec appendBookSolution a b = 
  match a with 
    [] -> b 
  | h::t -> h :: append t b;;

(* question, is my solution tail-recursive/better?*)

let rec rev l = 
  match l with 
    [] -> l 
  | h::t -> rev t @ [h];;

(* not very efficient, i.e. not tail recursive *)


let rec take n l = 
  if n = 0 then [] else 
    match l with 
      [] -> []
    | h::t -> h :: take (n-1) t

let rec drop n l =  
  if n = 0 then l else 
    match l with 
      [] -> l 
    | h::t -> drop (n - 1) t