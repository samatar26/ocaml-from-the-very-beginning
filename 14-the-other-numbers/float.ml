let make_vector ((x0, y0): float * float) ((x1, y1): float * float) : float * float = 
  (x1 -. x0, y1 -. y0)

let vector_length ((x,y): float * float) : float = 
  sqrt (x *. x +. y *. y)

let offset_point (x, y) (px, py) = 
  (px +. x, py +. y)

let scale_to_length l (a, b) =
  let currentlength = vector_length (a, b) in 
  if currentlength = 0. then (a, b) else 
    let factor = l /. currentlength in 
    (a *. factor, b *. factor)

(* Maybe go over vectors *)