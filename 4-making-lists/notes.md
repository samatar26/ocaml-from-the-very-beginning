```ocaml
let rec even l = 
  match l with 
    h::e::t -> e :: even t 
  | _ -> []

```
  t here can be an empty list, i.e. in the case of a 2 element list [1;2]
  (Need to double check.)

Cons operator is right associative, so this works: 
```ocaml

1 :: 2 :: 3 :: 4 :: [1]

```