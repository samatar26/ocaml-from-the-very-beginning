let rec take n l = 
  match l with 
    [] -> 
    if n = 0 then []
    else raise (Invalid_argument "take")
  | h::t -> 
    if n < 0 then raise (Invalid_argument "take") else 
    if n = 0 then []
    else h :: take (n-1) t

let rec drop n l = 
  match l with 
    [] -> 
    if n = 0 then []
    else raise (Invalid_argument "drop")
  | h::t -> 
    if n < 0 then raise (Invalid_argument "drop") else 
    if n = 0 then l 
    else drop (n-1) t

(* 
  Example of creating an exception
 *)

exception Problem 

exception NotPrime of int
(* raise (NotPrime x) *)

let f x = if x < 0 then raise Problem else 100 / x 

(* 
  Exceptions can also be handled. Exception handlers are written using the try ... with construct 
 *)

let safe_divide (x: int) (y: int) : int = 
  try x / y with 
    Division_by_zero -> 0

(* 
  Our function tries to divide x by y, but if the result of x / y
  raises the built-in exception Division_by_zero we return zero. 
  Therefore typewise, since the expression x / y has type int, 
  the expression we substitute in the case of a Division_by_zero exception must have the same type: int. 
 *)

let rec last l = 
  match l with 
    [] -> raise Not_found
  | [x] -> x 
  | _::t -> last t

(* 
  The type of a function gives no indication of what exceptions are likely to be raised or handled. 
  It's the responsibility of the programmer to ensure that exceptions which should be handled always are. 

  Later in the we'll see some alternatives to exceptions (Option?)
 *)