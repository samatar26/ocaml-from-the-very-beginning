### Recursive evaluation

```ocaml
let rec factorial a = 
  if a <= 0 then 0 else 
    if a = 1 then 1 else a * factorial (a - 1);;
```

factorial 4
=⇒ 4 * factorial 3
=⇒ 4 * (3 * factorial 2)
=⇒ 4 * (3 * (2 * factorial 1))
=⇒ 4 * (3 * (2 * 1))
=⇒ 4 * (3 * 2)
=⇒ 4 * 6
=⇒ 24

### Helpful helper functions 

```ocaml
let rec greatestCommonDivisor a b = 
  if b = 0 then a else greatesCommonDivisor b (a mod b);;  
```