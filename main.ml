open Ai
open Deck

type gamestate = {mutable pot:int; deck:deck; mutable table: card list;
                  players:player list; mutable bet: int}


let rec dealhands (d:deck) (p: player list): unit =
  match p with
  | [] -> ()
  | h::t -> h.cards <- Deck.pop d 2;
            dealhands d t

let dealcards (g: gamestate) (i: int): unit =
  g.table <- (Deck.pop g.deck i)

let printgame (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> print_string "Starting new hand"; print_newline ()
         (*Print each players score*)
  | _ -> print_string "Well, fuck"


let rec engine (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> printgame g;
         dealhands g.deck g.players;
         (*ASKFORBETS AND SHIT*)
         dealcards g 3;
         engine g

  | 3 -> printgame g;
         (*ASKFORBETS AND SHIT*)
         dealcards g 1;
         engine g

  | 4 -> printgame g;
         (*ASKFORBETS AND SHIT*)
         dealcards g 1;
         engine g

  | 5 -> printgame g
         (*ASKFORBETS AND SHIT*)
         (*Determine winner*)
         (*Distribute score to *)
  | _ -> failwith "Muhfucka"