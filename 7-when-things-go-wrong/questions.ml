(* 
  Questions
1. Write a function smallest which returns the smallest positive element of a list of integers. If there
is no positive element, it should raise the built-in Not_found exception.
2. Write another function smallest_or_zero which uses the smallest function but if Not_found is
raised, returns zero.
3. Write an exception definition and a function which calculates the largest integer smaller than or
equal to the square root of a given integer. If the argument is negative, the exception should be
raised.
4. Write another function which uses the previous one, but handles the exception, and simply returns
zero when a suitable integer cannot be found.
5. Comment on the merits and demerits of exceptions as a method for dealing with exceptional
situations, in contrast to returning a special value to indicate an error (such as -1 for a function
normally returning a positive number

I think the merit is that it's a lot more explicit using exceptions, instead of getting some random value back.
).
 *)

let rec smallest_inner current found l = 
  match l with 
    [] -> 
    if found then current else raise Not_found
  | h::t -> 
    if h > 0 && h < current 
    then smallest_inner h true t 
    else smallest_inner current found t 

let smallest (l: int list): int = 
  smallest_inner max_int false l 

let smallest_or_zero (l: int list): int = 
  try smallest l with 
    Not_found -> 0


(* 
  In number theory, the integer square root (isqrt) of a positive integer n is the positive integer m which is the greatest integer less than or equal to the square root of n,
  For example, isqrt 27 = 5, because 5*5 = 25 <= 27 and 6*6 = 36 > 27.
 *)
exception Negative_integer

let rec isqrt_inner x y = 
  if y*y <= x then y else isqrt_inner x (y-1)


let isqrt x = 
  if x < 0 then raise Negative_integer else isqrt_inner x x

let isqrt_or_zero x = 
  try isqrt x with 
    Negative_integer -> 0