open Random
open Deck
open Cardcomp

type status = Playing | Allin | Folded
type player = { mutable state : status ; mutable money : int;
                mutable cards : card list ; mutable currentbet : int;
                mutable best_hand : hand option; human : bool}

type action = Fold | Call | Raise of int

let create_human () = {state = Playing ; money = 10000; cards = []; currentbet = 0;
                 best_hand = None; human=true}
let create_ai () = {state = Playing ; money = 10000; cards = []; currentbet = 0;
                 best_hand = None; human=false}

let maxbet (playerlist : player list) : int =
  List.fold_left (fun acc x -> if x.currentbet > acc then x.currentbet
                               else acc) 0 playerlist

let rec everyone_same_bet (currentbet: int) (playerlist : player list) : bool =
  match playerlist with
  | [] -> true
  | h::t -> if h.state = Playing then (currentbet = h.currentbet) &&
                                       everyone_same_bet currentbet t
            else everyone_same_bet currentbet t

let decisionpreflop (player : player) (currentbet: int) : action =
  let randomval = Random.float 1. in
  match player.cards with
  | x::y::[] -> if (currentbet - player.currentbet) > player.money then ((player.state <- Allin);
                                (player.currentbet <- (player.currentbet + player.money));
                                (player.money <- 0);
                                Call)
                else (if x.value = y.value then (if randomval < 0.7 then
                                            (let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            (player.currentbet <- currentbet);
                                            Call)
                                            else (let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference)); (*has to match if the money is behind*)
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            (player.money <- (player.money - raiseval));
                                            (player.currentbet <- (currentbet+raiseval));
                                            Raise raiseval))
                      else (if randomval < 0.8 then
                            (let difference = currentbet - player.currentbet in
                            (player.money <- (player.money - difference));
                            (player.currentbet <- currentbet);
                            Call)
                            else
                            (let difference = currentbet - player.currentbet in
                            (player.money <- (player.money - difference));
                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                            (player.money <- (player.money - raiseval));
                            (player.currentbet <- (currentbet+raiseval));
                            Raise raiseval)))
  | _ -> failwith "error"

let allinhelper (player: player) (currentbet : int) : action =
                                let difference = currentbet - player.currentbet in
                                let old_money = player.money in
                                player.state <- Allin;
                                player.currentbet <- (player.currentbet + player.money);
                                player.money <- 0;
                                if difference > old_money then Call
                                else Raise (old_money - difference)

let foldhelper (player: player) : action =
                                player.state <- Folded;
                                Fold


let decisionflop (player: player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.1 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.2 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fourofkind _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.15 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.15 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fullhouse _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Flush _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Straight _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.35 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Threeofkind _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Twopair _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.5 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Onepair _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                              else (if randomval < 0.7 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Highcard _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                              else (if randomval < 0.85 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.03 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | _ -> failwith "error"

let decisionturn (player: player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.1 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.4 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fourofkind _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.15 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.3 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Fullhouse _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.2 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Flush _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.15 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Straight _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.35 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Threeofkind _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Twopair _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.6 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Onepair _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                              else (if randomval < 0.8 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Highcard _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                        else let difference = currentbet - player.currentbet in
                        player.money <- (player.money - difference);
                        player.currentbet <- currentbet;
                        Call
  | _ -> failwith "error"

let decisionriver (player : player) (currentbet : int) : action =
  let randomval = Random.float 1. in
  match player.best_hand with
  | Some (Straightflush _) -> allinhelper player currentbet
  | Some (Fourofkind _) -> allinhelper player currentbet
  | Some (Fullhouse _) -> allinhelper player currentbet
  | Some (Flush _) -> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.2 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.3 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Straight _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.3 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.2 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Threeofkind _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.4 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.1 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Twopair _)-> if (currentbet - player.currentbet) > player.money then allinhelper player currentbet
                              else (if randomval < 0.75 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Onepair _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                              else (if randomval < 0.9 then
                                            let difference = currentbet - player.currentbet in
                                            player.money <- (player.money - difference);
                                            player.currentbet <- currentbet;
                                            Call
                                            else let difference = currentbet - player.currentbet in
                                            (player.money <- (player.money - difference));
                                            let raiseval = int_of_float (0.05 *. (float_of_int player.money)) in
                                            player.money <- (player.money - raiseval);
                                            player.currentbet <- (currentbet+raiseval);
                                            Raise raiseval)
  | Some (Highcard _)-> if (currentbet - player.currentbet) > player.money then foldhelper player
                        else let difference = currentbet - player.currentbet in
                        player.money <- (player.money - difference);
                        player.currentbet <- currentbet;
                        Call
  | _ -> failwith "error"

let current_best_hand (player:player) : unit =
  player.best_hand <- Some (best_hand player.cards)

let winners (players: player list) : player list =
  let rec winnershelper (acc: player list) (playerslst: player list) =
    match (acc, playerslst) with
    | ([], h::t) -> winnershelper [h] t
    | (_, []) -> acc
    | (h::t, h1::t1) -> (match (h.best_hand), (h1.best_hand) with
                         | Some hand, Some hand1 -> let result = comparing hand hand1 in
                                                    if result = -1 then winnershelper [h1] t1
                                                    else if result = 1 then winnershelper acc t1
                                                    else winnershelper (h1::acc) t1 (*zero*)
                         | _ -> failwith "no hands?")
  in winnershelper [] players