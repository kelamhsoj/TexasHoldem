open Random
open Deck

type status = Playing | AllIn | Folded
type player = {mutable state: status; mutable money: int;
               mutable cards: card list; mutable currentbet: int}

type action = Fold | Call | Raise of int

let create () = {state=Playing; money=200; cards=[]; currentbet=0}

let decision (lst: card list) (score:int): action =
  let x = Random.int 2 in
  match x with
  | 0 -> Fold
  | 1 -> Call
  | _ -> failwith "error"

let initdecision (lst: card list): action =
  let x = Random.int 5 in
  match x with
  | 0 -> Fold
  | _ -> Call