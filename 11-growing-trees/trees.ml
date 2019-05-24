type 'a tree = 
    Br of 'a * 'a tree * 'a tree 
  | Lf 

(* 
  Our type is called a tree and is polymorphic (i.e. can hold any type of data at the branches).
  Two constructors, Br for branches which hold three things in a tuple, an element/the data, the left sub-tree and the right. 
  If there is no Br, it's a leaf which signals that there is no left or right sub tree. 
 *)

let rec size (tr: 'a tree) : int = 
  match tr with 
  | Br (_, lt, rt) -> 1 + size lt + size rt
  | Lf ->  0

(* Add all the integers in an int tree *)

let rec total (tr: int tree) : int = 
  match tr with 
  | Br(x, lt, rt) -> x + total lt + total rt 
  | Lf -> 0

let max x y = 
  if x > y then x else y

let rec maxdepth tr = 
  match tr with 
  | Br(x, lt, rt) -> 1 +  max (maxdepth lt)  (maxdepth rt) 
  | Lf -> 0 

(* 
  Note - GO BACK AND UNDERSTAND MAXDEPTH
 *)

let rec list_of_tree = function 
  | Br (x, lt, rt) -> x :: (list_of_tree lt) @  (list_of_tree rt) 
  | Lf -> []

let rec tree_map f = function 
  | Br(x, lt, rt) -> Br (f x, tree_map f lt, tree_map f rt)
  | Lf -> Lf

(* 
  If we arrange the tree such that, at each branch, everything to the left has a key less than the key at the branch
  and everything at the right has a key greater than that at the branch, we have a binary search tree.
 *)

let rec lookup k = function 
  | Br((k', v), lt, rt) -> 
    if k = k' then v
    else if k < k' then lookup k lt 
    else lookup k rt 
  | Lf -> raise Not_found 

let rec lookup k = function 
  | Br((k', v), lt, rt) -> 
    if k = k' then Some v
    else if k < k' then lookup k lt 
    else lookup k rt 
  | Lf -> None


let rec insert k v = function 
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), lt, rt) -> 
    if k = k' then Br ((k, v), lt, rt)
    else if k < k' then Br ((k', v'), insert k v lt, rt)
    else Br ((k', v'), lt, insert k v rt) 

(* 
  Lightbulb moment - Thought this wouldn't work because I had to reconstruct the tree completely, whereas this function stops as soon as it finds the right insertion point. 
  Thing is though, depending on the insertion point, the left and/or right sub trees remain intact. We're just replacing the value! 
 *)