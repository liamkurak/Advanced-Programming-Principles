(* Functions from section 1.5 *)

(* A parametric tree type. The tree type takes the parameter 'a
   to indicate what values are stored in the tree.

   These come from whirlwind_tree_parametric.ml`
 *)

type 'a tree = Empty
             | Fork of 'a tree * 'a * 'a tree
(* Note that we've moved the value element (the 'a value) into the
   middle position of the triple. In `whirlwind_tree_ parametric.ml
   the value was the first argument of the triple.

   The change is a meaningless one for OCaml but it lets us
   read the values in a tree from left to right and see that
   they are in ascending order.
 *)


(* Sample trees, of different types. *)
let t1 : int tree
  = Fork (Fork (Empty, 2, Empty),
          3,
          Fork (Fork (Empty, 4, Empty),
                5, 
                Fork (Empty, 6, Empty)
            )
      )

let t2 : string tree = 
  Fork (Fork (Empty, "2", Empty),
        "3",
        Fork (Fork (Empty, "4", Empty),
              "5", 
              Fork (Empty, "6", Empty)
          )
    )


let rec sum (t: int tree) : int =
  match t with
  | Empty -> 0
  | Fork (left, n, right) -> sum left + n + sum right

let rec concat (t: string tree) : string =
  match t with
  | Empty -> ""
  | Fork (left, n, right) -> concat left ^ n ^ concat right

let rec elem (v: 'a) (t: 'a tree) : bool =
  match t with
  | Empty -> false
  | Fork (left, n, right) ->
     if v = n 
     then true
     else if v < n
          then elem v left
          else elem v right


(* The 2-3-4 and Rose trees we describe here aer used simply to 
   show a different form of structured data. We are ignoring any 
   assumptions about the ordering of their values.  For the 
   'a tree type above, we did care about that ordering - as seen 
   in the `elem` function. But for these 2-3-4 trees, and for 
   Rose trees, we do not care about this ordering.
 *)

type 'a tree234 
  = Empty234
  | Fork2 of 'a * 'a tree234 * 'a tree234
  | Fork3 of 'a * 'a tree234 * 'a tree234 * 'a tree234
  | Fork4 of 'a * 'a tree234 * 'a tree234 * 'a tree234 * 'a tree234

let rec reduce234 (e: 'b) (f2: 'a -> 'b -> 'b -> 'b) 
          (f3 : 'a -> 'b -> 'b ->'b -> 'b) 
          (f4 : 'a -> 'b -> 'b ->'b -> 'b -> 'b)  
          (t: 'a tree234) : 'b =
  match t with
  | Empty234 -> e
  | Fork2 (n, t1 ,t2) -> f2 n (reduce234 e f2 f3 f4 t1) 
                           (reduce234 e f2 f3 f4 t2)
  | Fork3 (n, t1, t2, t3) -> f3 n (reduce234 e f2 f3 f4 t1) 
                               (reduce234 e f2 f3 f4 t2) 
                               (reduce234 e f2 f3 f4 t3)
  | Fork4 (n, t1, t2, t3, t4) -> f4 n (reduce234 e f2 f3 f4 t1) 
                               (reduce234 e f2 f3 f4 t2) 
                               (reduce234 e f2 f3 f4 t3)
                               (reduce234 e f2 f3 f4 t4)

let sum234_r (t: int tree234) : int
  = reduce234 0
      (fun n v1 v2 -> n + v1 + v2)
      (fun n v1 v2 v3 -> n + v1 + v2 + v3)
      (fun n v1 v2 v3 v4 -> n + v1 + v2 + v3 + v4)
      t


let t234 =
  Fork2 (4,
         Fork3 (3, Empty234, Fork2 (1, Empty234, Empty234), Empty234),
         Fork2 (6, Empty234, Empty234)
    )

let rec sum234 (t: int tree234) : int =
  match t with
  | Empty234 -> 0
  | Fork2 (v, t1, t2) -> v + sum234 t1 + sum234 t2
  | Fork3 (v, t1, t2, t3) -> v + sum234 t1 + sum234 t2 + sum234 t3
  | Fork4 (v, t1, t2, t3, t4) -> v + sum234 t1 + sum234 t2 +
                                   sum234 t3 + sum234 t4

type 'a rosetree =
  Rose of 'a * 'a rosetree list

let rec reduce_rose (r: 'a -> 'b list -> 'b) (t: 'a rosetree) : 'b =
  match t with
  | Rose (v, ts) -> r v (List.map (reduce_rose r) ts)

let sumRose_r (rt: int rosetree) : int =
  let f (n: int) (ns: int list) = List.fold_right (+) ns n
  in
  reduce_rose f rt

let rec sumRose (t: int rosetree) : int =
  match t with
  | Rose (v, ts) ->
     List.fold_left (+) v (List.map sumRose ts)

let rt1 : int rosetree 
  = Rose (4, [ Rose (3, []);
               Rose (5, [ Rose (6, []);
                          Rose (7, []);
                          Rose (8, []);
                        ] )
             ] )



let rec tree_map (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  match t with
  | Empty -> Empty
  | Fork (left, v, right) -> Fork (tree_map f left, f v, tree_map f right)

let rec tree_reduce (f: 'b -> 'a -> 'b -> 'b) (e: 'b) (t: 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (left, v, right) -> 
     f (tree_reduce f e left) v (tree_reduce f e right)

let sum_tree t = tree_reduce (fun lv x rv -> lv + x + rv) 0 t

let rec flatten (t: 'a tree) : 'a list =
  match t with 
  | Empty -> []
  | Fork (left, v, right) -> (flatten left) @ [v] @ (flatten right)


type 'a maybe = Nothing
              | Just of 'a

let head_m (lst: 'a list) : 'a maybe = 
  match lst with
  | [] -> Nothing
  | x::_ -> Just x

let add_one_to_front (ns: int list) : int =
  match head_m ns with
  | Nothing -> 0 
  | Just n -> n + 1

let head_o (lst: 'a list) : 'a option = 
  match lst with
  | [] -> None
  | x::_ -> Some x 

let add_two_to_front (ns: int list) : int =
  match head_o ns with
  | None -> 0
  | Some n -> n + 2


let minList (ns: int list) : int option =
  match ns with
  | [] -> None
  | x::xs -> Some (List.fold_left min x xs)

let add_one_to_min (ns: int list) : int =
  match minList ns with
  | Some x -> x + 1
  | None -> 0
                                      
type msg = StringMsg of string * int
         | BoolMsg of bool * int
         | FloatMsg of float * int

let sample_msgs : msg list = [
    StringMsg ("Hello", 234);     StringMsg ("World", 235);
    BoolMsg (true, 245);    BoolMsg (false, 248);
    FloatMsg (3.14, 260);     StringMsg ("Bye!", 280)
  ]
let msg_log (m: msg) : string =
  match m with
  | StringMsg  (s, ts) -> 
     "Msg at " ^ string_of_int ts ^ ": " ^ s
  | BoolMsg (b, ts) -> 
     "Msg at " ^ string_of_int ts ^ ": " ^ string_of_bool b
  | FloatMsg (f, ts) -> 
     "Msg at " ^ string_of_int ts ^ ": " ^ string_of_float f

let log_messages (msgs: msg list) : string =
  String.concat "\n" (List.map msg_log msgs)

let print_messages (msgs: msg list) =
  print_endline (log_messages msgs)


type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let isWorkDay (d: weekday) : bool =
  match d with
  | Mon | Tue | Wed | Thu | Fri -> true
  | Sat | Sun -> false


type coord = float * float
type circ_desc = coord * float
type tri_desc = coord * coord * coord
type sqr_desc = coord * coord * coord * coord


type shape = Circle of circ_desc 
           | Triangle of tri_desc 
           | Square of sqr_desc

let isSquare s =
  match s with
  | Square _ -> true
  | _ -> false
     

type nat = Zero
         | Succ of nat

