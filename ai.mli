(*Holds specs of decision made by AI *)
type action

(*Takes a hand and a score and returns an action *)
val decision: card list -> int -> action