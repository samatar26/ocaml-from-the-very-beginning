(* 
  Questions
1. Consider the expression
let x = ref 1 in let y = ref 2 in x := !x + !x; y := !x + !y; !x + !y
What references have been created? What are their initial and final values after this expression has
been evaluated? What is the type of this expression?
2. What is the difference between [ref 5; ref 5] and let x = ref 5 in [x; x] ?
3. Imagine that the for ... to ... do ... done construct did not exist. How might we create the
same behaviour?
4. What are the types of these expressions?
[|1; 2; 3|]
[|true; false; true|]
[|[|1|]|]
[|[1; 2; 3]; [4; 5; 6]|]
[|1; 2; 3|].(2)
[|1; 2; 3|].(2) <- 4
5. Write a function to compute the sum of the elements in an integer array.
6. Write a function to reverse the elements of an array in place (i.e. do not create a new array).
7. Write a function table which, given an integer, builds the int array array representing the multipli-
cation table up to that number. For example, table 5 should yield:
1 2 3 4 5
2 4 6 8 10
3 6 9 12 15
4 8 12 16 20
5 10 15 20 25
There is more than one way to represent this as an array of arrays; you may choose.
8. The ASCII codes for the lower case letters 'a' . . . 'z' are 97. . . 122, and for the upper case letters
'A' . . . 'Z' they are 65. . . 90. Use the built-in functions int_of_char and char_of_int to write func-
tions to uppercase and lowercase a character. Non-alphabetic characters should remain unaltered.
9. Comment on the accuracy of our character, word, line, and sentence statistics in the case of our
example paragraph. What about in general?
10. Choose one of the problems you have identified, and modify our program to fix it.
 *)


let q1 = 
  let x = ref 1 in 
  let y = ref 2 in
  x := !x + !x; 
  y := !x + !y; 
  !x + !y

(* 
  two int refs have been created for x and y. 
  x's initial value is 1. 
  y's initial value is 2. 
  x's final value is 2. 
  y's final value is 4. 
  The type of this expression is int.
 *)

(* 2. The expression [ref 5; ref 5;] is of type int ref list. Changing the contents of one reference will not change the contents of the other. 
      The expression let x = ref 5 in [x; x] is also of type int ref list, but changing the value of one will also change the other.
*)

let r = let x = ref 5 in [x;x]

let update_ref = function 
  | h::_ -> h:= 6
  | [] -> ()

let rec for_loop f n m = 
  if n <= m then 
    begin 
      f n;
      for_loop f (n + 1) m 
    end

(* 
  4. 
  int array 
  bool array 
  (int array) array 
  int list array 
  int 
  unit
 *)

let rec sum_int_array (a: int array) : int = 
  let sum = ref 0 in 
  Array.iter 
    (fun x -> sum := !sum + x) 
    a;
  !sum

let array_sum a =
  let sum = ref 0 in
  for x = 0 to Array.length a - 1 do
    sum := !sum + a.(x)
  done;
  !sum

let reverse_array a = 
  if a <> [||] then 
    let length = Array.length a - 1 in 
    for x = 0 to length / 2 do 
      let temp = a.(x) in 
      a.(x) <- a.(length - x);
      a.(length -x) <- temp ;
      print_endline "doing"
    done


let table n = 
  let a = Array.make n [||] in 
  for x = 0 to n - 1 do 
    a.(x) <- Array.make n 0 
  done;
  for x = 0 to n - 1 do 
    for y = 0 to n -1 do
      a.(x).(y) <- (x + 1) * (y + 1)
    done 
  done; 
  a

let transform_case c = 
  let char_code = int_of_char c  in 
  if char_code <= 122 && char_code >= 97 then 
    char_of_int (char_code - 32) else 
  if char_code <= 90 && char_code >= 65 then 
    char_of_int (char_code + 32)  
  else c 

(* 9. 
    Counting words by counting spaces is inaccurate as a line with ten words will only be counted as having nine words. 
    The number of characters doesn't include newlines. Periods, exclamation marks and questions marks may appear in multiples leading to a wrong answer. 
*)