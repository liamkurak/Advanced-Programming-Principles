type 'a circList
  = Nil 
  | Cons of 'a * 'a circList ref

let nil_list = ref Nil
let cl = Cons (1, 
               ref (Cons 
                      (2, ref 
                            (Cons (3, ref (Cons (4, 
                                   ref (Cons (5, nil_list)))))))))

let rec take (n: int) (cl: 'a circList) : 'a list =
  match n, cl with
  | 0, _ -> [] 
  | _, Nil -> [] 
  | _, Cons (hd, tl) -> hd :: take (n-1) (! tl)

let () = nil_list := cl
