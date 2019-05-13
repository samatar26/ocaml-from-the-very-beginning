# Insertion Sort

```ocaml 
let rec insert x l = 
  match l with 
    [] -> [x]
  | h::t -> 
    if x <= h 
    then x :: h :: t 
    else h:: insert x t 

let rec sort l = 
  match l with 
    [] -> []
  | h::t -> insert h (sort t)
```

Very useful evaluation order: 

sort [53; 9; 2; 6; 19]
∗ insert 53 (sort [9; 2; 6; 19])
∗ insert 53 (insert 9 (sort [2; 6; 19]))
∗ insert 53 (insert 9 (insert 2 (sort [6; 19])))
∗ insert 53 (insert 9 (insert 2 (insert 6 (sort [19]))))
∗ insert 53 (insert 9 (insert 2 (insert 6 (insert 19 (sort [])))))
∗ insert 53 (insert 9 (insert 2 (insert 6 (insert 19 []))))
∗ insert 53 (insert 9 (insert 2 (insert 6 [19])))
∗ insert 53 (insert 9 (insert 2 [6; 19]))
∗ insert 53 (insert 9 [2; 6; 19])
∗ insert 53 [2; 6; 9; 19]
∗ [2; 6; 9; 19; 53]

OCaml's comparison functions work for types other than **int**.

The runtime of _insertion sort_ is On^2. This is because each insertion operation takes time equal to the number of elements of the list (n). So let's say we have 4 elements in our list, each insertion operation will take 4 operations, 4*4 = 16 = 4^2 = n^2.

