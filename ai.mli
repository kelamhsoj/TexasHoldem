open Deck

type status = Playing | AllIn | Folded
type player = {mutable state: status; mutable money: int;
               mutable cards: card list; mutable currentbet: int}

(*Holds specs of decision made by AI *)
type action = Fold | Call | Raise of int

(*Takes a hand and a score and returns an action *)
val decision: card list -> int -> action