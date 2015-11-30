open Deck

type gamestate = {dealer: int; pot:int; scores:int ref list; deck: deck;
                  table: card list; hands: card*card list}

let printgame (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> print_string "Starting new hand"; print_newline ();
         for x = 1 to (List.length g.scores) do
           print_string ("Player " ^ (string_of_int x) ^ " has " ^ (string_of_int !(List.nth g.scores (x-1))) ^ " dollars"); print_newline ()
         done
  | _ -> print_string "Well, fuck"


let rec dealhands (deck: deck) scores: card*card list =
  match scores with
  | [] -> []
  | h::t -> (Deck.pop deck 2) @ (dealhands deck t)


let engine (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> printgame g;
         dealhands deck hands;
         if g.dealer = 0 then

  (*If no cards on table, get bets and stuff, turn over 3 cards*)
  | 3 -> printgame g;
  (*If 3 cards on table, more bets, turn over 4th card*)
  | 4 -> printgame g;
  (*If 4 cards on table, final bets, turn over 5th*)
  | 5 -> printgame g;
  (*If 5 cards on table, determine who wins, distribute funds*)
  | _ -> failwith "Something went wrong"


let _ =
  let newgamestate = {dealer=1; pot=0; scores=[ref 100; ref 100]; table=[]; hands=[]} in
  engine newgamestate