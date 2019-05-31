(* A program which takes sufficiently to run so that we can distinguish between 
   then ocamlc and ocamlopt compilers.
   NOTE - You can use time in your terminal to see the runtime of a program.*)

let _ = 
  let i = ref 0 in
  for x = 0 to 100000000 do 
    i := 1 + !i
  done;
  print_int !i;
  print_newline ()