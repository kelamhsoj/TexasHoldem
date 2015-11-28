open Deck

(*Takes a list of cards and returns the best 5 card combination*)
val best_hand: card list -> card list

(*Converts a 5 card hand to an integer value based on power of the the hand*)
val hand_value: card list -> int

(*Returns true if hand one is better than hand two*)
val is_better_hand: card list -> card list -> bool