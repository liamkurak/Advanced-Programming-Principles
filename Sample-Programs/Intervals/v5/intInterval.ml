
open Intervals

module Int_comparable = struct
  type t = int
  let compare = compare 
  let to_string = string_of_int
end

module Int_interval = Make_interval (Int_comparable)

(* The following line causes an error.

   We've provided 'create' the values of type 'int' and this is the
   same as Int.t' which is the same as EndPoint.t.

   But 'endpoint' in Int_interval is bound to the type
   'Make_interval(Core.Std.Int).endpoint'

   We've failed to indicate that the endpoint in Int_interval is
   related to the type in the Int module that is input to the
   Make_interval functor.

   We also need to make sure this type is not hidden.

Try the following in utop after loading this, or uncomment it here to see the
error when using ocamlbuild.

let i = Int_interval.create 3 4

 *)


