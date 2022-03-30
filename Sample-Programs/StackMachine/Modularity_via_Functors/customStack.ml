
module CustomStackM : Stack.StackS = struct

  type 'a t = Empty
            | Node of 'a * 'a t

  let empty = Empty

  let peek s = match s with
    | Empty -> raise (Failure "empty stack")
    | Node (top, _) -> top

  let push v s = Node (v, s)

  let pop s = match s with
    | Empty -> raise (Failure "empty stack")
    | Node (top, rest) -> (top, rest)

end
