open Deck
open Ai

type gamestate = {dealer: int; mutable pot:int; mutable scores:int ref list;
                  deck: deck; table: card list; hands: (card*card) list;
                  mutable players: int list; bets: int ref list}
(*Players are numbered 0 through n-1 where n is the total number of players*)


let printgame (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> print_string "Starting new hand"; print_newline ();
         for x = 1 to (List.length g.scores) do
           print_string ("Player " ^ (string_of_int x) ^ " has " ^ (string_of_int !(List.nth g.scores (x-1))) ^ " dollars"); print_newline ()
         done
  | _ -> print_string "Well, fuck"


let get_user_input (prompt: string): string =
  print_string prompt;
  read_line()


let get_action (input: string): string =
  let word_list = Str.split (Str.regexp " ") input in
  if List.length word_list = 1
    then String.lowercase input
  else
    String.lowercase (List.nth word_list 0)

let bet (g: gamestate) (bet: int) (player: int) =
  g.pot := g.pot + bet;
  let currentbet = !(List.nth g.bets player) in
  (List.nth g.bets player) := currentbet + bet


let fold (g: gamestate) (i: int): unit =
  if not List.mem i g.players then failwith ("Player " ^ string_of_int i ^ " cannot fold")
  else
    let newplayers = List.filter (fun a -> a != i) g.players in
    g.players := newplayers;


let nextplayer (g: gamestate) (i:int): int =
  if (i+1) = (List.length !g.players)
    then 0
    else i + 1


let askaround (g: gamestate) (init: int) (i: int) (bet: int): unit =
  let looped = False in
  while looped = False do
    let hand = (List.nth g.hands i) in
    match (Ai.decision hands) with
    | Fold -> fold g i;
    | Call -> let newbet = (bet - !(List.nth g.bets i)) in
              bet g newbet i;
    let newi = nextplayer g i in
    if newi = init then looped = True else askaround g init newi bet
  done


let rec dealhands (g: gamestate) (p: players): unit =
  match p with
  | [] -> ()
  | h::t -> let ind = (List.length g.players) - (List.length p) in
            (List.nth g.hands ind) := (Deck.pop g.deck 2);
            dealhands g t


let dealcards (g: gamestate) (i: int): gamestate =
  let newtable = (Deck.pop g.deck i) in
  {dealer=g.dealer; pot=g.pot; scores=g.scores; deck=g.deck; table=newtable;
   hands=g.hands; players=g.players; bets=g.bets}


let rec engine (g: gamestate): unit =
  match (List.length g.table) with
  | 0 -> printgame g;
         dealhands g;
         if g.dealer = 0 then
            bet g 20 0;
            askaround g 1 20;
            let input = get_user_input "Check or Raise?" in
            let action = get_action input in
            match action with
            | "check" -> let newg = dealcards g 3 in
                         engine newg
            | "raise" -> let word_list = Str.split (Str.regexp " ") input in
                         let newbetstring = List.nth word_list 1 in
                         let newbet = int_of_string newbetstring in
                         bet g newbet 0; askaround g 1 newbet;
                         let newg = dealcards g 3 in
                         engine newg
         else
            bet g 20 g.dealer;
            askaround g (nextplayer g.dealer) 20;
            let newg = dealcards g 3 in
            engine newg

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