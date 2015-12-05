open Ai
open Deck
open Str

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
  players= [human; ai1; ai2; ai3; ai4]; bet = 100; mode = Welcome } in
  newgamestate

let add_cardto_table gstate =
  let old = gstate.table in
  match gstate.mode with
  | Flop -> let newc = pop gstate.deck 3 in
            gstate.table <- newc
  | Turn | River -> let newc = pop gstate.deck 1 in
                    gstate.table <- old @ newc
  | _ -> failwith "error"


(*Prints promp and returns the action option of what the human wants to do*)
let cycle_user_input (prompt: string): action option =
  print_string prompt;
  print_newline ();
  let result = read_line() in
  let result' = String.lowercase result in
  let word_list = Str.split (Str.regexp " ") result' in
  match word_list with
  | x::[] -> (match x with
           | "fold" -> Some Fold
           | "check" -> Some Call
           | "call" -> Some Call
           | _ -> None)
  | x::y::tl -> (match x with
              | "raise" -> (try (let num = int_of_string y in
                               Some (Raise num))
                           with _ -> None)
              | _ -> None)
  | _ -> None

(*Reads the gamestate, asks the player the appropritate question, and returns an action option*)
let getuserdecision gstate player =
  if (gstate.bet > player.currentbet) then cycle_user_input "Would you like to fold, call, or raise the bet further?"
  else if (gstate.bet = player.currentbet) then cycle_user_input "Would you like to check or raise?"
  else failwith "error"

(*Prints out a list of cards*)
let rec printcardlist = function
  | [] -> ()
  | [x] -> printcard x
  | h::t -> printcard h; print_string ", ";
            printcardlist t

(*Method for testing that prints out the cards in each player's hand*)
let printhands gstate =
  for x = 0 to ((List.length gstate.players)-1) do
    print_string ("Player " ^ (string_of_int x) ^ ":");
    printcardlist (List.nth gstate.players x).cards;
    print_newline ()
  done

(*Welcomes User, ask if they would like to play*)
let rec welcome_user gstate =
  print_endline "Welcome to Texas Holdem!";
  print_endline "Ready to play against 4 Players?";
  let rec input_helper () =
    let result = read_line() in
    let result' = String.lowercase result in
    match result' with
      | "yes" -> print_endline "Ok, starting game! You are Player 0";
                gstate.mode <- Init
      | "no" -> print_endline "Ok, quitting"
               (*quit*)
      | _ -> print_endline "It was a yes or no question.";
             input_helper () in
  input_helper ();
  engine gstate

(* *Deal players two cards
   *Print to human what cards they have
  * Deduct buy-in score and add to pot
  * change gamestate mode to Preflop
 *)
and deal (gstate: gamestate) =
  print_endline "Dealing Cards";
  for x = 0 to ((List.length gstate.players)-1) do
    let p = (List.nth gstate.players x) in
    p.cards <- (Deck.pop gstate.deck 2);
    if p.human then (print_endline "Your hand is:"; printcardlist p.cards;
                print_newline ()) else ()
  done;
  gstate.mode <- Preflop;
  engine gstate

and run_cycle gstate =
  if (everyone_same_bet gstate.bet gstate.players) <> true then cycle gstate
  else if gstate.mode = Preflop then begin (gstate.mode <- Flop);  engine gstate end
  else if gstate.mode = Flop then begin (gstate.mode <- Turn) ;  engine gstate end
  else if gstate.mode = Turn then begin (gstate.mode <- River);  engine gstate end
  else if gstate.mode = River then begin (gstate.mode <- End) ;  engine gstate end
  else failwith "done"


(*Go through player list
  If player is a human, prompt user input (call, fold, or raise)
  Parse the input
  Update gamestate
  If player is an ai, get decision, match on decision, update gamestate
 *)
and cycle_human_helper gstate p =
  match (getuserdecision gstate p) with
    | None -> print_endline "Command not recognized. Please try again";
              cycle_human_helper gstate p
    | Some Fold -> print_endline "You fold"; p.state <- Folded
    | Some Call -> (let rest = (gstate.bet - p.currentbet) in
                   if rest > p.money then
                   (p.currentbet <- (p.currentbet + p.money);
                   gstate.pot <- gstate.pot + p.money;
                   p.money <- 0;
                   p.state <- Allin)
                   else (p.currentbet <- (p.currentbet + rest);
                         gstate.pot <- gstate.pot + rest;
                         p.money <- p.money - rest))
    | Some Raise y -> (if y <=0 then
                      (print_endline "Invalid amount. Please try again";
                                            cycle_human_helper gstate p)
                      else if y >= p.money then
                      (p.currentbet <- p.currentbet + p.money;
                                      gstate.pot <- gstate.pot + p.money;
                                      p.money <- 0)
                      else (let rest = (gstate.bet - p.currentbet) in
                                      p.currentbet <- (gstate.bet + y);
                                      gstate.pot <- gstate.pot + rest + y;
                                      p.money <- ((p.money - rest) - y)))
and cycle gstate =
  let rec cycleinside gstate players =
  match players with
  | [] -> ()
  | h :: t ->
    (match h.state with
    | Folded -> print_endline ("Player is still out because she folded"); cycleinside gstate t
    | Allin ->  print_endline ("Player is still all in"); cycleinside gstate t
    | Playing -> if h.human = true then begin cycle_human_helper gstate h; cycleinside gstate t end
                 else let bet = h.currentbet in
                     (match gstate.mode with
                      | Preflop -> (match Ai.decisionpreflop h gstate.bet with
                                    | Fold -> print_endline ("Player folds"); cycleinside gstate t
                                    | Call -> print_endline ("Player calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                                  cycleinside gstate t)
                      | Flop -> (match Ai.decisionflop h gstate.bet with
                                    | Fold -> print_endline ("Player folds"); cycleinside gstate t
                                    | Call -> print_endline ("Player calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                              cycleinside gstate t)
                      | Turn -> (match Ai.decisionturn h gstate.bet with
                                    | Fold -> print_endline ("Player folds"); cycleinside gstate t
                                    | Call -> print_endline ("Player calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y)
                      | River -> (match Ai.decisionriver h gstate.bet with
                                    | Fold -> print_endline ("Player folds"); cycleinside gstate t
                                    | Call -> print_endline ("Player calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                              cycleinside gstate t)
                      | _ -> failwith "Non-Playable State"))
in begin (cycleinside gstate gstate.players); run_cycle gstate end

(* Hands are shown, pot is emptied, score of winner is updated
  If any players are eliminated, they are removed from the table
  If the human has no money they are prompted to play again
  If they do have more money they are asked if they want to play a new round
  and stage goes back to init
*)
and finish_round gstate =
  for x = 0 to ((List.length gstate.players)-1) do
    let p = (List.nth gstate.players x) in
    match p.state with
      | Folded -> ()
      | Allin  -> failwith "TODO"
      | Playing -> failwith "TODO"

  done;
  print_endline "Would you like to play another round?";
  let rec input_helper () =
    let result = read_line() in
    let result' = String.lowercase result in
    match result' with
      | "yes" -> print_endline "Ok, starting new round!";
                gstate.mode <- Init
      | "no" -> print_endline "Ok, quitting"
               (*quit*)
      | _ -> print_endline "It was a yes or no question.";
             input_helper () in

  input_helper ();
  engine gstate


(*Main function that runs based on state of the game*)
and engine gstate =
(*add print statements when cards are added*)
  match gstate.mode with
    | Welcome -> welcome_user gstate
    | Init -> deal gstate
    | Preflop -> cycle gstate
    | Flop -> print_string "Flop: "; add_cardto_table gstate;
    printcardlist gstate.table;
    print_newline();
    cycle gstate
    | Turn -> print_string "Turn: "; add_cardto_table gstate;
    printcardlist gstate.table;
    print_newline();
    cycle gstate
    | River -> print_string "River: "; add_cardto_table gstate;
    printcardlist gstate.table;
    cycle gstate
    | End -> finish_round gstate

let _ =
  engine (init_game () )
