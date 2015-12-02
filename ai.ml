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

let decisionpreflop (player : player) (currentbet: int) : action =
  let randomval = Random.float 1. in
  match player.cards with
  | x::y::[] -> if currentbet > player.money then ((player.state <- Allin);
                                (player.currentbet <- (player.currentbet + player.money));
                                (player.money <- 0);
                                Call)
                else (if x.value = y.value then (if randomval < 0.7 then
                                            (let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            (player.currentbet <- currentbet);
                                            Call)
                                            else (let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            (player.money <- (player.money - raiseval));
                                            (player.currentbet <- (currentbet+raiseval));
                                            Raise raiseval))
                      else (if randomval < 0.8 then
                            (let difference = currentbet - player.currentbet in
                            (player.money <- (player.money - difference));
                            (player.currentbet <- currentbet);
                            Call)
                            else
                            (let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                            (player.money <- (player.money - raiseval));
                            (player.currentbet <- (currentbet+raiseval));
                            Raise raiseval)))
  | _ -> failwith "error"

let allinhelper (player: player) (currentbet : int) : action =
                                player.state <- Allin;
                                player.currentbet <- (player.currentbet + player.money);
                                player.money <- 0;
                                Call


let decisionflop (player: player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.1 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.2 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fourofkind _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.15 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.15 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fullhouse _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Flush _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Straight _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.35 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Threeofkind _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Twopair _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.5 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Onepair _)-> if currentbet > player.money then Fold
                              else (if randomval < 0.7 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Highcard _)-> if currentbet > player.money then Fold
                              else (if randomval < 0.85 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = int_of_float (0.03 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)

let decisionturn (player: player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.1 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.4 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Fourofkind _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.15 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.3 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Fullhouse _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.2 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Flush _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.15 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Straight _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.35 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.1 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Threeofkind _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.05 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Twopair _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.6 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.05 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Onepair _)-> if currentbet > player.money then Fold
                              else (if randomval < 0.8 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.05 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Highcard _)-> if currentbet > player.money then Fold
                              else (if float_of_int (player.money) *. 0.7 < currentbet then Fold
                                    else Call)


let decisionriver (player : player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> allinhelper player currentbet
  | Some (Fourofkind _) -> allinhelper player currentbet
  | Some (Fullhouse _) -> allinhelper player currentbet
  | Some (Flush _) -> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.3 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Straight _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.2 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Threeofkind _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.1 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Twopair _)-> if currentbet > player.money then allinhelper player currentbet
                              else (if randomval < 0.75 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.05 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Onepair _)-> if currentbet > player.money then Fold
                              else (if randomval < 0.9 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- player.money - difference;
                                            player.currentbet <- currentbet;
                                            Call
                                            else let raiseval = 0.05 *. (float_of_int player.money) in
                                            player.money <- player.money - raiseval;
                                            player.currentbet <- currentbet+raiseval;
                                            Raise (int_of_float raiseval))
  | Some (Highcard _)-> if currentbet > player.money then Fold
                              else (if float_of_int (player.money) *. 0.5 < currentbet then Fold
                                    else Call)

let initdecision (lst: card list): action =
  let x = Random.int 5 in
  match x with
  | 0 -> Fold
  | _ -> Call

let current_best_hand (player:player) : unit =
  player.best_hand <- Some (best_hand cards)