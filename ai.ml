open Random
open Deck
open Cardcomp

type status = Playing | Allin | Folded
type player = { mutable state : status ; mutable money : int;
                mutable cards : card list ; mutable currentbet : int;
                mutable best_hand : hand option}

type action = Fold | Call | Raise of int

let create () = {state = Playing ; money = 200; cards = []; currentbet = 0;
                 best_hand = None}

let maxbet (playerlist : player list) : int =
  List.fold_left (fun acc x -> if x.currentbet > acc then x.currentbet
                               else acc) 0 playerlist
let rec everyone_same_bet (currentbet: int) (playerlist : player list) : bool =
  match playerlist with
  | [] -> true
  | h::t -> if h.state = Playing then (currentbet = h.currentbet) &&
                                       everyone_same_bet currentbet t
            else everyone_same_bet currentbet t

let percentagebet (best_combo : hand) : float =
  match best_combo with
  | Straightflush _ -> 1.0
  | Fourofkind _ -> 0.95
  | Fullhouse _ -> 0.85
  | Flush _ -> 0.7
  | Straight _-> 0.65
  | _ -> Random.float 0.5

let decision (h: hand) (score:int): action =
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