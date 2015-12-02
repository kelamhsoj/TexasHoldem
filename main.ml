open Ai
open Deck

(*Notes:
  * Game is initialized with a newgamestate
  * There are seven stages in the game:
  1. Welcome- User asked if they would like to play a game of Texas
  Holdem and that they will face four other players
  2.Init- Players must be dealt their two cards
  3. Preflop, Flop, Turn, River- For each stage go around the table and raise,
  call, or fold until every player has t he same bet(or is out). Then
  move to next stage
  4. End - Hands are shown, pot is emptied, score of winner is updated
  If any players are eliminated, they are removed from the table
  If the human has no money they are prompted to play again
  If they do have more money they are asked if they want to play a new round
  and stage goes back to init


  For consistency, most functions should take a gamestate and return unit
*)


type stage = Welcome| Init |Preflop |Flop |Turn |River |End
type gamestate = {mutable pot:int; deck:deck; mutable table: card list;
                  mutable players: player list; mutable bet: int; mutable mode: stage}

(*Create new gamestate*)
let init_game () =  let human = create_human () in
  let ai1 = create_ai () in
  let ai2 = create_ai () in
  let ai3 = create_ai () in
  let ai4 = create_ai () in
  let playingdeck = newdeck () in
  let newgamestate = {pot = 0; deck = playingdeck; table=[];
  players= [human; ai1; ai2; ai3; ai4]; bet = 0; mode = Welcome } in
  newgamestate

(*Prints promp and returns the action option of what the human wants to do*)
let cycle_user_input (prompt: string): action option =
  failwith "broken"
  (*print_string prompt;
  print_newline ();
  let result = read_line() in
  let result' = String.lowercase result in
  let word_list = Str.split (Str.regexp " ") result' in
  match word_list with
  | [x] -> match x with
           | "fold" -> Some Fold
           | "check" -> Some Call
           | "call" -> Some Call
           | _ -> None
  | x::[y] -> match x with
              | "raise" -> try let num = int_of_string y in
                               Some (Raise num)
                           with None
              | _ -> None
  | _ -> None*)

(*Welcomes User, ask if they would like to play*)
let welcome_user gstate =
  print_endline "Welcome to Texas Holdem!"


(* *Deal players two cards
   *Print to human what cards they have
  * Deduct buy-in score and add to pot
  * change gamestate mode to Preflop
 *)
let deal (gstate: gamestate) =
  print_endline "Dealing Cards";
  match (gstate.players) with
    | [] -> ()
    | h::t -> failwith "TODO"

(*Go through player list
  If player is a human, prompt user input (call, fold, or raise)
  Parse the input
  Update gamestate
  If player is an ai, get decision, match on decision, update gamestate
 *)
let rec cycle gstate =
  failwith "TODO"

(* Hands are shown, pot is emptied, score of winner is updated
  If any players are eliminated, they are removed from the table
  If the human has no money they are prompted to play again
  If they do have more money they are asked if they want to play a new round
  and stage goes back to init
*)
let finish_round gstate =
  failwith "TODO"

(*Main function that runs based on state of the game*)
let engine gstate =
  match gstate.mode with
    | Welcome -> welcome_user gstate
    | Init -> deal gstate
    | Preflop -> cycle gstate
    | Flop -> (*Add a card to the table*) cycle gstate
    | Turn -> (*Add a card to the table*) cycle gstate
    | River -> (*Add a card to the table*) cycle  gstate
    | End -> finish_round gstate

let _ =
  engine (init_game () )
