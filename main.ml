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
type gamestate = {mutable pot:int; mutable deck: deck; mutable table: card list;
                  mutable players: player list; mutable bet: int; mutable mode: stage}

(*Create new gamestate*)
let init_game () =  let human = create_human () in
  let ai1 = create_ai "Sandy" in
  let ai2 = create_ai "Fred" in
  let ai3 = create_ai "Jeff" in
  let ai4 = create_ai "Terry" in
  let playingdeck = newdeck () in
  let newgamestate = {pot = 0; deck = playingdeck; table=[];
  players= [human; ai1; ai2; ai3; ai4]; bet = 5; mode = Welcome } in
  newgamestate

let add_cardto_table gstate =
  let old = gstate.table in
  match gstate.mode with
  | Flop -> let newc = pop gstate.deck 3 in
            gstate.table <- newc;
            for x = 0 to ((List.length gstate.players)-1) do
              let p = List.nth gstate.players x in
              p.cards <- p.cards @ newc
            done
  | Turn | River -> let newc = pop gstate.deck 1 in
                    gstate.table <- old @ newc;
                    for x = 0 to ((List.length gstate.players)-1) do
                      let p = List.nth gstate.players x in
                      p.cards <- p.cards @ newc
                    done
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
  else (*if (gstate.bet = player.currentbet) then*) cycle_user_input "Would you like to check or raise?"
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
      | "yes" -> print_endline "Ok, starting game! You are Player 1";
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
  gstate.bet <- (maxbet gstate.players);
  if (everyone_same_bet (gstate.bet) gstate.players) then begin
    match gstate.mode with
    | Preflop -> gstate.mode <- Flop; gstate.bet <- 0;
    for x = 0 to (List.length gstate.players -1) do
    let p = List.nth gstate.players x in
    p.currentbet <- 0
    done;  engine gstate
    | Flop -> gstate.mode <- Turn; gstate.bet <- 0;
    for x = 0 to (List.length gstate.players -1) do
    let p = List.nth gstate.players x in
    p.currentbet <- 0
    done;  engine gstate
    | Turn -> gstate.mode <- River; gstate.bet <- 0;
    for x = 0 to (List.length gstate.players -1) do
    let p = List.nth gstate.players x in
    p.currentbet <- 0
    done;  engine gstate
    | River -> gstate.mode <- End;  engine gstate
    | _ -> failwith "Run only works on playable parts"
  end
else cycle gstate

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
                      else if (gstate.bet + y >= p.money) then
                      begin           p.currentbet <- p.currentbet + p.money;
                                      gstate.pot <- gstate.pot + p.money;
                                      p.state <- Allin;
                                      gstate.bet <- p.money;
                                      p.money <- 0
                                                   end
                      else (let rest = (gstate.bet - p.currentbet) in
                                      p.currentbet <- (gstate.bet + y);
                                      gstate.pot <- gstate.pot + rest + y;
                                      gstate.bet <- gstate.bet + y;
                                      p.money <- ((p.money - rest) - y)))

and cycleinside gstate players =
  match players with
  | [] -> ()
  | h :: t -> (match h.state with
    | Folded -> print_endline (h.name ^ " " ^ h.pronoun ^ " still out because she folded"); cycleinside gstate t
    | Allin ->  print_endline (h.name ^ " " ^ h.pronoun ^ " still all in"); cycleinside gstate t
    | Playing -> if h.human = true then
                 begin cycle_human_helper gstate h; cycleinside gstate t end
                 else let bet = h.currentbet in
                     (match gstate.mode with
                      | Preflop -> (match Ai.decisionpreflop h gstate.bet with
                                    | Fold -> print_endline (h.name ^ " folds"); cycleinside gstate t
                                    | Call -> print_endline (h.name ^ " calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> print_endline (h.name ^ " raises " ^ string_of_int y ^ " dollars");
                                                 gstate.pot <- (gstate.pot + (gstate.bet - bet) + y);
                                                 gstate.bet <- gstate.bet + y;
                                                 cycleinside gstate t)
                      | Flop ->    current_best_hand h; (*compute the best hand*)
                                   (match Ai.decisionflop h gstate.bet with
                                    | Fold -> print_endline (h.name ^ " folds"); cycleinside gstate t
                                    | Call -> print_endline (h.name ^ " calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> print_endline (h.name ^ " raises " ^ string_of_int y ^ " dollars");
                                                 (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                                 cycleinside gstate t)
                      | Turn ->    current_best_hand h; (*compute the best hand*)
                                   (match Ai.decisionturn h gstate.bet with
                                    | Fold -> print_endline (h.name ^ " folds"); cycleinside gstate t
                                    | Call -> print_endline (h.name ^ " calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> print_endline (h.name ^ " raises " ^ string_of_int y ^ " dollars");
                                                 (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                                  cycleinside gstate t)
                      | River -> current_best_hand h; (*computes the best hand*)
                                   (match Ai.decisionriver h gstate.bet with
                                    | Fold -> print_endline (h.name ^ " folds"); cycleinside gstate t
                                    | Call -> print_endline (h.name ^ " calls");
                                              gstate.pot <- gstate.pot + (gstate.bet - bet);
                                              cycleinside gstate t
                                    | Raise y -> print_endline (h.name ^ " raises " ^ string_of_int y ^ " dollars");
                                                 (gstate.pot <- gstate.pot + (gstate.bet - bet) + y);
                                                  gstate.bet <- gstate.bet + y;
                                                  cycleinside gstate t)
                      | _ -> failwith "Non-Playable State"))

and cycle gstate =
  let playerlst = gstate.players in
  ((cycleinside gstate playerlst)); run_cycle gstate

(* Hands are shown, pot is emptied, score of winner is updated
  If any players are eliminated, they are removed from the table
  If the human has no money they are prompted to play again
  If they do have more money they are asked if they want to play a new round
  and stage goes back to init
*)

and finish_round gstate =

  for x = 0 to ((List.length gstate.players) -1) do
    let p = List.nth gstate.players x in
    current_best_hand p
    done;

  (*make sure everyone has the most updated hands*)
  let best_hands = winners (gstate.players) in
  (*find the winners*)

  let split_pot = (gstate.pot) / (List.length best_hands) in
  (*split it amongst the number of people*)

  for x = 0 to ((List.length best_hands) -1) do
    let p = List.nth best_hands x in
    (p.money <- p.money + split_pot)
  done;

  let rec remove_nomoney acc lst =
  match lst with
  | [] -> acc
  | h :: t -> if (h.money = 0) then remove_nomoney acc t else
              remove_nomoney (h::acc) t in

  let updated_playerlist = remove_nomoney [] gstate.players in

  let player_not_bust = List.fold_left (fun acc x -> acc || x.human) false updated_playerlist in
  if player_not_bust then
    if List.length updated_playerlist = 1 then
     (print_endline "You won!!!! Mazal tov!";
      print_endline "Would you like to play another game?";
    let rec input_helper () =
      let result = read_line() in
      let result' = String.lowercase result in
      match result' with
        | "yes" -> print_endline "Ok, starting new game!";
                let newstate = init_game () in
                newstate.mode <- Init;
                engine newstate
        | "no" -> print_endline "Ok, quitting"
               (*quit*)
        | _ -> print_endline "It was a yes or no question.";
               input_helper () in
  input_helper ())
   else (print_endline "Would you like to play another round?";
    let rec input_helper () =
      let result = read_line() in
      let result' = String.lowercase result in
      match result' with
        | "yes" -> print_endline "Ok, starting new game!";
                gstate.pot <- 0;
                gstate.players <- updated_playerlist;
                for x = 0 to (List.length gstate.players -1) do
                let p = List.nth gstate.players x in
                p.state <- Playing;
                p.cards <- [];
                p.best_hand <- None;
                p.currentbet <- 0
                done;
                gstate.deck <- newdeck();
                gstate.bet <- 100;
                gstate.mode <- Init;
                gstate.table <- [];
                engine gstate
        | "no" -> print_endline "Ok, quitting"
               (*quit*)
        | _ -> print_endline "It was a yes or no question.";
               input_helper () in
  input_helper ())
  else
   (print_endline "You have lost all your money." ;
    print_endline "Would you like to play again?" ;
     let rec input_helper () =
       (let result = read_line() in
       let result' = String.lowercase result in
       match result' with
        | "yes" -> print_endline "Ok, starting new game!";
                let newstate = init_game () in
                newstate.mode <- Init;
                engine newstate
        | "no" -> print_endline "Ok, quitting"
        | _ -> print_endline "It was a yes or no question.";
               input_helper ()) in
    input_helper () )

(*Main function that runs based on state of the game*)
and engine gstate =
(*add print statements when cards are added*)
  match gstate.mode with
    | Welcome -> welcome_user gstate
    | Init -> deal gstate
    | Preflop -> cycle gstate
    | Flop -> print_string "Flop: "; add_cardto_table gstate;
    printcardlist gstate.table;
    print_newline(); cycle gstate
    | Turn -> print_string "Turn: "; add_cardto_table gstate;
    printcardlist gstate.table;
    print_newline();
    cycle gstate
    | River -> print_string "River: "; add_cardto_table gstate;
    printcardlist gstate.table;
    print_newline();
    cycle gstate
    | End -> finish_round gstate

let _ =
  engine (init_game () )
