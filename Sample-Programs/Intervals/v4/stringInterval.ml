
(* Here use use the 'Make_interval' functor to create a module
   with types and functions for string intervals. *)

open Intervals

module String_comparable = struct
  type t = string
  let compare = compare 
  let to_string x = x
end

module String_interval = Make_interval (String_comparable)



