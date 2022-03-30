(* This code is the same as in Chapter 9 of Real World OCaml,
   except that we've added a 'to_string' function to the
   signature and modules.
 *)

(* We first create a signature, sometimes called an interface,
   indicating what types and values a module must have, at least,
   to be used as an end point in an interval. 
 *)
module type Comparable = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end


(* The Make_interval functor takes a module that matches the
   Comparable signature and uses it to create an interval module over
   the endpoint defined by that input module.

   The result is transparent, we don't constraint what is visible
   through the use of a signature on the result.
  *)
module Make_interval(Endpoint : Comparable) = struct

  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  type endpoint = Endpoint.t

  (* 'create low high' creates a new interval from 'low' to
     'high'.  If 'low > high', then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  (* Returns true iff the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  (* 'contains t x' returns true iff 'x' is contained in the
     interval 't' *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
       Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  (* 'intersect t1 t2' returns the intersection of the two input
     intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1,t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1,h1), Interval (l2,h2) ->
       create (max l1 l2) (min h1 h2)

  let to_string i : string =
    match i with
    | Empty -> "Empty"
    | Interval (l,h)
      -> "(" ^ Endpoint.to_string l ^ ", " ^ Endpoint.to_string h ^ ")"

end 


