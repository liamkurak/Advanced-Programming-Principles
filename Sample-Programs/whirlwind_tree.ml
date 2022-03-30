(* A binary tree data type *)
type tree = Empty
          | Fork of int * tree * tree

(* A sample trees *)
let t1 : tree 
  = Fork (3,
          Fork (2, Empty, Empty),
          Fork (5, 
                Fork (4, Empty, Empty),
                Fork (6, Empty, Empty)
            )
      )


(* Pictorially, t1 is:
         3
        / \
       2   5
          / \
         4   6
*)



(* Adding up all the numbers in a tree.*)
let rec sum (t: tree) : int =
  match t with
  | Empty -> 0
  | Fork (n, left, right) -> n + sum left + sum right

(* Checking if an integer is in a tree. *)
let rec elem (i: int) (t: tree) : bool =
  match t with
  | Empty -> false
  | Fork (n, left, right) -> 
     if i = n
     then true
     else if i < n 
          then elem i left
          else elem i right

(* We can make new trees using existing trees. The new
   tree shares some components with other trees.
  *)
let t2 = Fork (7, t1, Empty)

(* Pictorially, t2 is:
           7
          /
         3
        / \
       2   5
          / \
         4   6
*)


(* Inserting a number into a tree to maintain the "ordered"
   nature of the tree.  This is not a balanced tree, however.

   The new tree will share many components with the original
   one.
 *)
let rec insert (i: int) (t: tree) : tree = 
  match t with
  | Empty -> Fork (i, Empty, Empty)
  | Fork (n, left, right) ->
     if i = n then t
     else if i < n then Fork (n, insert i left, right)
                   else Fork (n, left, insert i right)

let t3 = insert 7 t1             

(* Pictorially, t3 is:
         3
        / \
       2   5
          / \
         4   6
              \
               7
*)
