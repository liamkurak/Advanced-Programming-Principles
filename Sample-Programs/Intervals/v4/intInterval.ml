
(* Here use use the 'Make_interval' functor to create a module
   with types and functions for integer intervals. *)

open Intervals

module Int_comparable = struct
  type t = int
  let compare = compare 
  let to_string = string_of_int
  let square x = x * x
end

module Int_interval = Make_interval (Int_comparable)

let i = Int_interval.create 3 4
