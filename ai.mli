open Deck
open Cardcomp

type status = Playing | Allin| Folded
type player = { mutable state : status ; mutable money : int;
                mutable cards : card list ; mutable currentbet : int;
                mutable best_hand: hand option}
(*Holds specs of decision made by AI *)
type action = Fold | Call | Raise of int

val create: unit -> player

val maxbet: player list -> int

val everyone_same_bet: int -> player list -> bool

(*Takes a hand and a score and returns an action *)
val decision: hand -> int -> action
