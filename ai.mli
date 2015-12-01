open Deck

(*Holds specs of decision made by AI *)
type action = Fold | Call | Raise of int

(*Takes a hand and a score and returns an action *)
val decision: card list -> int -> action