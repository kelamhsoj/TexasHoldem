open Deck

(*Takes a list of cards and returns the best 5 card combination*)
val best_hand: card list -> card list

(*returns 1 if first hand is better, 0 if equal, -1 if worse
based on standard texas hold'em rules*)
val compare: card list -> card list -> int