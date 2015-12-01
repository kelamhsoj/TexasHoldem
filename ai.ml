open Random

type action = Fold | Call | Raise of int

let decision (lst: card list): action =
  let x = Random.int 2 in
  match x with
  | 0 -> Fold
  | 1 -> Call

let initdecision (lst: card list): action =
  let x = Random.int 5 in
  match x with
  | 0 -> Fold
  | _ -> Check