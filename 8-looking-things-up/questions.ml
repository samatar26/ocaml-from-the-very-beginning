(* 
  Questions
1. Write a function to determine the number of different keys in a dictionary.
2. Define a function replace which is like add , but raises Not_found if the key is not already there.
3. Write a function to build a dictionary from two equal length lists, one containing keys and another
containing values. Raise the exception Invalid_argument if the lists are not of equal length.
4. Now write the inverse function: given a dictionary, return the pair of two lists – the first containing
all the keys, and the second containing all the values.
5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are found, the value
associated with the first occurrence of the key should be kept.
6. Write the function union a b which forms the union of two dictionaries. The union of two dictio-
naries is the dictionary containing all the entries in one or other or both. In the case that a key is
contained in both dictionaries, the value in the first should be preferred.
 *)

let rec contains k d = 
  match d with 
    [] -> false 
  | (k', v')::t -> k = k' || contains k t


let rec unique_keys_inner d n l = 
  match d with 
    [] -> n 
  | (k, v)::t -> 
    if contains k l
    then unique_keys_inner t n l 
    else  unique_keys_inner t (n+1) ((k, v)::l) 

let unique_keys d = 
  unique_keys_inner d 0 []

(* 
  Since the keys must be unique in a dictionary, number of different keys is simple the length of the list
  representing the dictionary, so we can just use the usual length function. 
 *)

let rec replace k v d = 
  match d with 
    [] -> raise Not_found
  | (k', v')::t -> 
    if k = k' 
    then (k, v) :: t 
    else (k', v') :: replace k v t

let rec mkdict keys values = 
  match keys, values with 
    [], [] -> []
  | [], _ -> raise (Invalid_argument "mkdict")
  | _, [] -> raise (Invalid_argument "mkdict")
  | k::ks, v::vs -> (k, v) :: mkdict ks vs


let rec mklist d = 
  match d with 
    [] -> ([], [])
  | (k, v)::t -> 
    let (ks, vs) = mklist t in 
    (k::ks, v::vs)

(* 
  See book solution again using inner match. 
 *)

let rec member x l = 
  match l with 
    [] -> false 
  | h::t -> h = x || member x t 

let rec dictionary_of_pairs_inner keys_seen l = 
  match l with 
    [] -> []
  | (k, v)::t ->
    if member k keys_seen 
    then dictionary_of_pairs_inner keys_seen t
    else (k, v) :: dictionary_of_pairs_inner (k::keys_seen) t


let dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l

let rec add k v d = 
  match d with 
    [] -> [(k, v)]
  | (k', v')::t -> 
    if k = k' 
    then (k,v)::t 
    else (k', v') :: add k v t 

let rec union a b =
  match a with
    [] -> b
  | (k, v)::t -> add k v (union t b)

(* 
  Go back over union!
 *)