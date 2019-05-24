(* 
  Questions
1. Write a function of type α → α tree → bool to determine if a given element is in a tree.
2. Write a function which flips a tree left to right such that, if it were drawn on paper, it would appear
to be a mirror image.
3. Write a function to determine if two trees of the same type have the same shape, irrespective of
actual values of the elements. Can you write a more general version which can tell if two trees have
the same shape even if one has type α tree and one has type β tree for some α and β ?
4. Write a function tree_of_list which builds a tree representation of a dictionary from a list
representation of a dictionary.
5. Write a function to combine two dictionaries represented as trees into one. In the case of clashing
keys, prefer the value from the first dictionary.
6. Can you define a type for trees which, instead of branching exactly two ways each time, can branch
zero or more ways, possibly different at each branch? Write simple functions like size , total , and
map using your new type of tree.
 *)

type 'a tree =  
    Br of 'a * 'a tree * 'a tree 
  | Lf

let rec member_tree x = function 
  | Br (y, lt, rt) -> x = y || member_tree x lt || member_tree x rt 
  | Lf -> false


let rec flip_tree  = function 
  | Br (x, lt, rt) -> Br (x, flip_tree rt, flip_tree lt)
  | Lf -> Lf

let rec equal_shape tx ty = 
  match tx, ty with 
  | Br (_, lx, rx), Br(_, ly, ry) -> equal_shape lx ly && equal_shape rx ry
  | Lf, Lf -> true 
  | _, _ -> false

let rec tree_map f = function 
  | Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf

let equal_shape tr tr2 = 
  tree_map (fun _ -> 0) tr = tree_map (fun _ -> 0) tr2

let rec insert k v = function 
  | Br ((k', v'), l, r) ->  
    if k = k' then Br ((k, v), l, r)
    else if k < k' then Br ((k', v'), insert k v l, r)
    else Br((k', v'), l, insert k v r)
  | Lf -> Br((k, v), Lf, Lf)

let rec tree_of_list = function 
  | (k, v)::t -> insert k v (tree_of_list t)
  | [] -> Lf 


let rec merge_trees tr tr2 = 
  match tr with 
  | Br((k, v), l, r) -> insert k v (merge_trees (merge_trees l r) tr2)
  | Lf -> Lf 

(* 
  Note - Check if my solution's any good. 
 *)

let rec list_of_tree = function 
  | Br (x, lt, rt) -> x :: (list_of_tree lt) @  (list_of_tree rt) 
  | Lf -> []

let rec merge_trees t t' = 
  tree_of_list (list_of_tree t @ list_of_tree t')


type 'a mtree = 
    Br of 'a * 'a mtree list 


let rec map f  = function 
  | h::t -> f h :: map f t 
  | [] -> []

let rec sum = function 
  | h::t -> h + sum t 
  | [] -> 0

let rec size = function 
  | Br (e, l) -> 1 + sum (map size l)

(* 
  When there's an empty array in the case of Br (10, []), map won't call the size function and will simply return an empty array.
 *)

let rec total tr =
  match tr with
    Br (e, l) -> e + sum (map total l)

let rec map_mtree f tr =
  match tr with
    Br (e, l) -> Br (f e, map (map_mtree f) l)

(* 
  When there is only one pattern to match, you can put it directly in place of the function's argument.
 *)

let rec size (Br (e, l)) =
  1 + sum (map size l)

let rec total (Br (e, l)) =
  e + sum (map total l)

let rec map_mtree f (Br (e, l)) =
  Br (f e, map (map_mtree f) l)