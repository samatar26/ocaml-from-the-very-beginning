(* 
Questions
1. Write a function evens which does the opposite to odds , returning the even numbered elements in a
list. For example, evens [2; 4; 2; 4; 2] should return [4; 4] . What is the type of your function?
2. Write a function count_true which counts the number of true elements in a list. For example,
count_true [true; false; true] should return 2 . What is the type of your function? Can you
write a tail recursive version?
3. Write a function which, given a list, builds a palindrome from it. A palindrome is a list which
equals its own reverse. You can assume the existence of rev and @ . Write another function which
determines if a list is a palindrome.
4. Write a function droplast which returns all but the last element of a list. If the list is empty, it
should return the empty list. So, for example, droplast [1; 2; 4; 8] should return [1; 2; 4] .
What about a tail recursive version?
5. Write a function member of type α → α list → bool which returns true if an element exists in a list,
or false if not. For example, member 2 [1; 2; 3] should evaluate to true , but member 3 [1; 2]
should evaluate to false .
6. Use your member function to write a function make_set which, given a list, returns a list which
contains all the elements of the original list, but has no duplicate elements. For example, make_set
[1; 2; 3; 3; 1] might return [2; 3; 1] . What is the type of your function?
7. Can you explain why the rev function we defined is inefficient? How does the time it takes to run
relate to the size of its argument? Can you write a more efficient version
 *)


let rec evens (l: 'a list) : 'a list = 
  match l with 
    _::e::t -> e :: evens t 
  | _ -> []

let rec count_true (l:bool list): int = 
  match l with
    [] -> 0 
  | true::t -> 1 + count_true t 
  | false::t -> count_true t 

let rec count_true_inner (l: bool list) (n: int) : int = 
  match l with 
    [] -> n
  | true::t -> count_true_inner t (n + 1)
  | false::t -> count_true_inner t n

(* 
  Interesting/annoying bug: 
  if you do count_true_inner t n + 1 instead of count_true_inner t (n+1),
  you end up with a non-tail recursive function that still works as you're adding + 1
  for every function call where you find a true. 

 *)
let count_true_tail_recursive (l: bool list) = 
  count_true_inner l 0

(* 
To make a palindrome from any list we need to append the list to its reverse.
 *)

let rec rev (l: 'a list) : 'a list= 
  match l with 
    [] -> []
  | h::t -> rev t @ [h]

let build_palindrome l: 'a list = 
  l @ rev l


let is_palindrome (l: 'a list) : bool = 
  rev l = l

let rec drop_last (l: 'a list) : 'a list = 
  match l with 
    [] -> []
  | [_] -> []
  | h::t -> h :: drop_last t

let rec drop_last_inner (a: 'a list) (l: 'a list) : 'a list = 
  match l with 
    [] ->  List.rev a
  | [_] ->  List.rev a
  | h::t -> drop_last_inner (h::a) t

let drop_last_tail_recursive (l: 'a list): 'a list = 
  drop_last_inner [] l

let rec member (e: 'a) (l: 'a list) : bool = 
  match l with
    [] -> false
  | h::t -> h = e || member e t

let rec make_set (l: 'a list): 'a list = 
  match l with 
    [] -> []
  | h::t -> if member h t then make_set t else h :: make_set t 

(* 
7. The first part of the evaluation of rev takes time proportional to the length of the list, 
  processing each element once. However, when the lists are appended together, the order of the operations is such that the first argument
  becomes longer each time. 

  The @ operator as we know takes time proportional to the length of its first argument. 
  Therefore the accumulating of the lists takes time proportional to the square of the length of the list. 
 *)

let rec rev_inner l a = 
  match l with 
    [] -> a 
  | h::t -> rev_inner t (h::a)   

let rev_v2 (l: 'a list) : 'a list =
  rev_inner l []

(* 
    This version operates in time proportional to the length of the list
    and is also tail-recursive I believe.
   *)