open Deck
open Cardcomp

type status = Playing | Allin| Folded
type player = { mutable state : status ; mutable money : int;
                mutable cards : card list ; mutable currentbet : int;
                mutable best_hand: hand option ; human : bool; name: string;
                pronoun: string}
(*Holds specs of decision made by AI *)
type action = Fold | Call | Raise of int

val create_human: unit -> player

val create_ai: string -> player

val maxbet: player list -> int

val everyone_same_bet: int -> player list -> bool

(*Takes a hand and a score and returns an action *)
val decisionpreflop: player -> int -> action

(*Takes a hand and a score and returns an action *)
val decisionflop: player -> int -> action

(*Takes a hand and a score and returns an action *)
val decisionturn: player -> int -> action

(*Takes a hand and a score and returns an action *)
val decisionriver: player -> int -> action

val current_best_hand: player -> unit

val winners: player list -> player list

val printplayer: player -> unit

val printplayerscore: player -> unit