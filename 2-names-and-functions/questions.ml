(* Questions
   1. Write a function which multiplies a given number by ten. What is its type?
   2. Write a function which returns true if both of its arguments are non-zero, and false otherwise.
   What is the type of your function?
   3. Write a recursive function which, given a number n, calculates the sum 1 + 2 + 3 + . . . + n. What is
   its type?
   4. Write a function power x n which raises x to the power n . Give its type.
   5. Write a function isconsonant which, given a lower-case character in the range 'a' . . . 'z' , deter-
   mines if it is a consonant.
   6. What is the result of the expression let x = 1 in let x = 2 in x + x ?
      4 as the value assigned to the name x is coming from the nearest let expression let x = 1 in (let x = 2 in x + x)
   7. Can you suggest a way of preventing the non-termination of the factorial function in the case of
   a zero or negative argument?
   We can simply return 0 for a negative or zero argument.
*)

let multiplyByTen (x: int) : int = x * 10

let nonZero (x: int) (y: int): bool = 
  x <> 0 && y <> 0

let rec sum n = 
  if n = 1 then 1 else n + sum (n -   1)

let rec power (x:int) (n:int) : int = 
  if n = 0 then 1 else x * power x (n - 1)

let not x = if x then false else true

let isVowel (x: char) : bool = x = 'a' || x = 'e' || x = 'u' || x = 'i' || x = 'o'

let isConsonant (x:char) : bool = not (isVowel x)

