
(* Instead of using ``open`` to make all the contents of a module
   immediately accessible one can create a short abbreviation or
   synonym for a module so that using this name is less cumbersome.

   Below, we just write ``I.`` to access the components of the
   ``Intervals`` module.
 *)

module I = Intervals

module Int_comparable : (I.Comparable with type t = int) = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module Int_interval = I.Make_interval(Int_comparable)

(* The following line now works. *)
let i = Int_interval.create 3 4

