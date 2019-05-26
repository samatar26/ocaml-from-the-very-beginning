(* 
  Questions
1. Give a function which rounds a positive floating-point number to the nearest whole number,
returning another floating-point number.
2. Write a function to find the point equidistant from two given points in two dimensions.
3. Write a function to separate a floating-point number into its integer and whole parts. Return them
as a tuple of type float × float.
4. Write a function star of type float → unit which, given a floating-point number between zero and
one, draws an asterisk to indicate the position. An argument of zero will result in an asterisk in
column one, and an argument of one an asterisk in column fifty.
5. Now write a function plot which, given a function of type float → float, a range, and a step size,
uses star to draw a graph. For example, assuming the existence of the name pi for π, we might see:
OCaml
# plot sin 0. pi (pi /. 20.);;
*
  *
    *
      *
        *
          *
            *
              *
                *
                  *
                  *
                  *
                *
              *
            *
          *
        *
      *
    *
  *
*
Here, we have plotted the sine function on the range 0 . . . π in steps of size π/20. You can define pi
by calculating 4.0 *. atan 1.0 .
 *)

let round x = 
  let c = ceil x  in 
  let f = floor x in 
  if c -. x <= x -.f then c else f

let midway_point (x0, y0) (x1, y1) = 
  ((x0 +. x1) /. 2., y0 +.  (y0 +. y1) /. 2.)

let rec parts x =
  if x < 0. then 
    let (a,b) = parts( -. x) in 
    (-.a , b) else 
    let whole_number = floor x in 
    (whole_number, x -. whole_number)


let star x = 
  let pos = int_of_float (floor (x *. 50.)) in 
  for x = 1 to pos - 1 do 
    print_char ' '
  done;
  print_char '*';
  print_newline ()


let plot f a b step = 
  let pos = ref a in 
  while !pos <= b do 
    star (f !pos); 
    pos := !pos +. step 
  done