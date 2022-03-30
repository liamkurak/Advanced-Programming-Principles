
(* The directive ``open`` makes all the contents of the opened module
   accessible directly so that the dot notation is not needed to access
   module components.
 *)
open Intervals
open IntInterval
open StringInterval

let i1 = Int_interval.create 3 4

let i2 = Int_interval.create 3 6

let s1 = String_interval.create "a" "d"

let () = 
  print_endline ("An interval: " ^ Int_interval.to_string i1) ;

  print_endline ("Another interval: " ^ Int_interval.to_string i2) ;

  print_endline ("Their intresection: " ^ 
		   Int_interval.to_string (Int_interval.intersect i1 i2)) ;

  print_endline ("A string interval: " ^ String_interval.to_string s1)



