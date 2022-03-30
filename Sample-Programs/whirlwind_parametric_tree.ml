(* A parametric tree type. The tree type takes the parameter 'a
   to indicate what values are stored in the tree.
 *)
type 'a tree = Empty
             | Fork of 'a * 'a tree * 'a tree


(* Sample trees, of different types. *)
let t1 : int tree
  = Fork (3,
          Fork (2, Empty, Empty),
          Fork (5, 
                Fork (4, Empty, Empty),
                Fork (6, Empty, Empty)
            )
      )

let t2 : string tree = 
  Fork ("3",
        Fork ("2", Empty, Empty),
        Fork ("5", 
              Fork ("4", Empty, Empty),
              Fork ("6", Empty, Empty)
          )
    )

let rec sum (t: int tree) : int =
  match t with
  | Empty -> 0
  | Fork (n, left, right) -> n + sum left + sum right

let rec concat (t: string tree) : string =
  match t with
  | Empty -> ""
  | Fork (n, left, right) ->
     n ^ concat left ^ concat right

let rec elem (v: 'a) (t: 'a tree) : bool =
  match t with
  | Empty -> false
  | Fork (n, left, right) ->
     if v = n 
     then true
     else if v < n
          then elem v left
          else elem v right

(* A dictionary, where tree elements are key/value pairs, here
   the key is an integer and the value is a string. 
 *)
let dictionary : (int * string) tree 
  = Fork ( (3,"three"), 
           Fork ( (2,"two"), Empty, Empty),
           Fork ( (4,"four"), 
                  Empty,
                  Fork ( (5,"five"), Empty, Empty)
            )
      )


(* You might load this file and understand why the values 
   of r1 and r2 are what they are.
 *)
let r1 = elem (3,"two") dictionary
let r2 = elem (3,"three") dictionary



(* An alternative version of the element function that takes two
   functions as argument. One, named `eq` that determines what it 
   means to find a value in the tree, and another named `lt` for 
   performaing comparisions for looking in the left or right subtrees.
 *)

let rec elem' (v: 'a) (eq: 'a -> 'a -> bool) (lt: 'a -> 'a -> bool) 
              (t: 'a tree) : bool =
  match t with
  | Empty -> false
  | Fork (n, left, right) ->
     if eq v n 
     then true
     else if lt v n
          then elem' v eq lt left
          else elem' v eq lt right


(* You might load this file and understand why the values 
   of va and vb are what they are.
 *)
let va = elem' 3 (fun a b -> a = b) (fun a b -> a < b) t1
let vb = elem' (3,"") 
           (fun (k1,v1) (k2,v2) -> k1 = k2)
           (fun (k1,v1) (k2,v2) -> k1 < k2)
           dictionary


(* "Folding up" or "reducing" a tree. *)

let rec reduce (e: 'b) (f: 'a -> 'b -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (v, lt, rt) -> f v (reduce e f lt) (reduce e f rt)

let sumr (t: int tree) : int = reduce 0 (fun v lv rv -> v + lv + rv) t

let concatr (t: string tree) : string = reduce "" (fun v lv rv -> v ^ lv ^ rv) t


(* Creating a new tree of the same "shape" but with new values. *)
let rec map (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  match t with
  | Empty -> Empty
  | Fork (v, lt, rt) -> Fork (f v, map f lt, map f rt)

let inc (n: int) (t: int tree) : int tree =
  let inc x = x + n in map inc t
