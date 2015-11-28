open Deck

(*
 * card represents the highest card in the hand-combination. If the highest
  * card is the same, and the combo is the same, then the two hands are
 * equivalent
 * Invariant: the first Card of Hand represents:
  1. for fourkind - the four of kind
  2. for fullhouse- the three of kind
  3. for straight - the largest card
  4. for two pair - the largest pair
  second card represents second pair
  last card represents the kicker
  5. for one pair - the largest pair
  the cardlist represents the kicker
  *)

type hand =
  | Straightflush of suits*card
  | Fourofkind of card*card
  | Fullhouse of card*card
  | Flush of suits *(card list)
  | Straight of card
  | Threeofkind of card*(card list)
  | Twopair of card * card * card
  | Onepair of card * (card list)
  | Highcard of card list


(*returns 1 if first hand is larger, 0 if equal, -1 if smaller*)
let compare firsthand secondhand =
  failwith "TODO"
(* match firsthand with
   | Straightflush (s,c) -> (match secondhand with
                               | Straightflush (s',c') -> _
                               | _ -> 1 )
   | Fourofkind (card, card) -> (match secondhand with
                                | )
   | _ -> failwith "TODO"*)

let best_hand clist =
  failwith "TODO"

let hand_value clist =
  failwith "TODO"

let is_better_hand clist1 clist2 =
  failwith "TODO"

let card_to_points  (c:card) =
  match c.value with
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4
    | Three -> 3
    | Two -> 2

let is_flush clist =
  let rec helper cards s =
    match cards with
      | [] -> true
      | h::t -> if h.suit = s then true && helper t s else false in

  helper clist ((List.hd clist).suit)

