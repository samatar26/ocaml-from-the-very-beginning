let rec string_in_line term line pos = 
  pos + String.length term <= String.length line 
  && 
  (String.sub line pos (String.length term) = term 
   || string_in_line term line (pos + 1))


let getlines filename = 
  let channel = open_in filename in 
  let lines = ref [] in 
  try 
    while true do 
      lines := input_line channel :: !lines 
    done; 
    []
  with 
    End_of_file -> 
    close_in channel; 
    List.rev !lines

let _ = 
  match Sys.argv with 
    [|_; searchterm; filename|] -> 
    begin
      try 
        List.iter 
          (fun line -> 
             if string_in_line searchterm line 0 then 
               print_endline line
          )
          (getlines filename)
      with 
        e ->  
        print_string "An error occurred:  ";
        print_endline (Printexc.to_string e);
        exit 1 
    end 
  | _ -> 
    print_endline "Usage: search <search_term> <input_filename>";
    exit 1