open Random
open Deck

type action = Fold | Call | Raise of int

let decision (lst: card list) (score:int): action =
  (* let x = Random.int 2 in
  match x with
  | 0 -> Fold
  | 1 -> Call
  | _ -> failwith "error" *)
  Call

let initdecision (lst: card list): action =
  let x = Random.int 5 in
  match x with
  | 0 -> Fold
  | _ -> Call