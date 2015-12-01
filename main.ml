open Str
open Deck
open Ai

type gamestate = {dealer: int; mutable pot: int;
      scores:int ref list; deck: deck;
      mutable table: card list; mutable hands: (card*card) list;
      mutable players: int list; bets: int ref list}
(*Players are numbered 0 through n-1 where n is the total number of players*)

let rec printcardlist = function
  | [] -> ()
  | h::t -> printcard (fst h); print_string ", "; printcard (snd h);
            print_newline (); printcardlist t

let rec printintlist = function
  | [] -> ()
  | h::t -> print_string (string_of_int h); print_newline ();
            printintlist t

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

let betfunction (g: gamestate) (bet: int) (player: int) =
  g.pot <- g.pot + bet;
  let currentbet = !(List.nth g.bets player) in
  (List.nth g.bets player) := currentbet + bet

let nextplayer (g: gamestate) (i:int): int =
  if (i+1) >= (List.length g.players) then 0 else (i + 1)

let createlist h tab =
  match h with
  | (x, y) -> x::y::tab

let tupleconv l =
  match l with
  | x::y::[] -> (x,y)
  | _ -> failwith "error"

let rec dealhands (g: gamestate) (p: int list): unit =
  match (List.length p) with
  | 0 -> ()
  | _ -> g.hands <- (tupleconv (Deck.pop g.deck 2))::g.hands;
         dealhands g (List.tl p)

let dealcards (g: gamestate) (i: int): gamestate =
  let newtable = (Deck.pop g.deck i) in
  {dealer=g.dealer; pot=g.pot; scores=g.scores; deck=g.deck; table=newtable;
   hands=g.hands; players=g.players; bets=g.bets}

let rec fold (g: gamestate) (i: int): unit =
  (if List.mem i g.players then
    (print_string ("Player " ^ (string_of_int i) ^ " folds");
    print_newline ();
    let newplayers = List.filter ((<>) i) g.players in
    g.players <- newplayers;
    printintlist g.players;
    if (List.length g.players = 1)
      then (let p = List.nth g.players 0 in
           match p with
           | 1 -> (print_string "Congratulations, you win the hand.";
                  print_newline ();
                  let score1 = !(List.nth g.scores 0) in
                  let newscore = score1 + g.pot in
                  g.pot <- 0;
                  (List.nth g.scores 0) := newscore;
                  g.table <- []);
           | _ -> (print_string ("Player " ^ (string_of_int p) ^ " has won the hand");
                  print_newline ();
                  let score1 = !(List.nth g.scores (p-1)) in
                  let newscore = score1 + g.pot in
                  g.pot <- 0;
                  (List.nth g.scores (p-1)) := newscore;
                  g.table <- []);
      engine g)
      else ())
  else failwith ("Player " ^ string_of_int i ^ " cannot fold"))

and askaround (g: gamestate) (init: int) (i: int) (bet: int): unit =
  (let looped = ref false in
  while !looped = false do
    let hand = List.nth g.hands i in
    match (Ai.decision (createlist hand []) bet) with
    | Fold -> fold g i;
              let newi = nextplayer g i in
              if newi = init then looped := true
              else askaround g init newi bet
    | Call -> let newbet = (bet - !(List.nth g.bets i)) in
              betfunction g newbet i;
              let newi = nextplayer g i in
              if newi = init then looped := true else askaround g init newi bet
    | _ -> failwith "error"
    done)

and checkorraise (g: gamestate): gamestate =
  (let input = get_user_input "Check or Raise?" in
  let action = get_action input in
  match action with
  | "check" -> dealcards g 3
  | "raise" -> let word_list = Str.split (Str.regexp " ") input in
               let newbet = int_of_string (List.nth word_list 1) in
               betfunction g newbet 0;
               askaround g 0 1 newbet;
               dealcards g 3
  | _ -> print_string "Invalid input. Please try again"; print_newline ();
         checkorraise g)

and engine (g: gamestate): unit =
  (match (List.length g.table) with
  | 0 -> printgame g;
         dealhands g g.players;
         if g.dealer = 0 then
            (betfunction g 20 0;
            askaround g 0 1 20;
            let newg = checkorraise g in
            engine newg)
         else
            betfunction g 20 g.dealer;
            askaround g (nextplayer g g.dealer) (nextplayer g g.dealer) 20;
            let newg = dealcards g 3 in
            engine newg

  (*If no cards on table, get bets and stuff, turn over 3 cards*)
  | 3 -> printgame g;
  (*If 3 cards on table, more bets, turn over 4th card*)
  | 4 -> printgame g;
  (*If 4 cards on table, final bets, turn over 5th*)
  | 5 -> printgame g;
  (*If 5 cards on table, determine who wins, distribute funds*)
  | _ -> failwith "Something went wrong")


let _ =
  let newgamestate = {dealer=0; pot=0; scores=[ref 100; ref 100];
                      deck=(Deck.newdeck ()); table=[]; hands=[];
                      players=[0;1];bets=[ref 0; ref 0]} in
  engine newgamestate
