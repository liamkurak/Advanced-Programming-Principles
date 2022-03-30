module ListStackM : Stack.StackS = struct
  type 'a t = 'a list

  let empty = []
  let peek s = match s with
    | [] -> raise (Failure "empty stack")
    | top::_ -> top
  let push v s = v :: s
  let pop s = match s with
    | [] -> raise (Failure "empty stack")
    | top::rest -> (top, rest)
end
