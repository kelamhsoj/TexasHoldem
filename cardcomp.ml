open Deck

(* card represents the highest card in the hand-combination. If the highest
  * card is the same, and the combo is the same, then the two hands are
 * equivalent
 * Invariant: the first Card of Hand represents:
  1. for fourkind - the four of kind
  2. for fullhouse- the three of kind
  3. for straight - the largest card
  4. for two pair - the largest pair
  second card represents second pair
  last card represents the kicker
  5. for one pair - the largest pair
  the cardlist represents the kicker
  6. for all the contructors that have a card
  list the cards are sorted from greatest to
  least based on their values.
  *)

type hand =
  | Straightflush of suits*card
  | Fourofkind of card*card
  | Fullhouse of card*card
  | Flush of suits *(card list)
  | Straight of card
  | Threeofkind of card*(card list)
  | Twopair of card * card * card
  | Onepair of card * (card list)
  | Highcard of card list


let best_hand clist =
  failwith "TODO"

let hand_value clist =
  failwith "TODO"

let is_better_hand clist1 clist2 =
  failwith "TODO"

(*Turns a card to an integer value based on its value *)
let card_to_points  (c:card) : int =
  match c.value with
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4
    | Three -> 3
    | Two -> 2


(*Returns the card list as an int list*)
let cardlist_to_points  (clist:card list) : int list =
  List.map (fun c -> card_to_points c) clist

(* precondition: the lsts are of the same size
 * both list are sorted from greatest to least*)
let rec cardlistcompare lst1 lst2 =
  match (lst1, lst2) with
  | ([],[]) -> 0
  | (h::t , h'::t') -> if (card_to_points h) > (card_to_points h') then 1
                       else if (card_to_points h) < (card_to_points h') then -1
                       else cardlistcompare t t'
  | (_,_) -> failwith "precondition: the list are of the same length"

(*returns 1 if first hand is larger, 0 if equal, -1 if smaller*)
let compare (firsthand :hand) (secondhand: hand) =
  let number = card_to_points in
  match firsthand with
  | Straightflush (_,c) ->
                  (match secondhand with
                   | Straightflush (_,c') ->
                   (if (number c) > (number c') then 1
                   else if (number c) < (number c') then -1
                   else 0)
                   | _ -> 1 )
  | Fourofkind (card4, card1) ->
                  (match secondhand with
                   | Straightflush _ -> -1
                   | Fourofkind (card4', card1')->
                   (if (number card4) > (number card4') then 1
                   else if (number card4) < (number card4') then -1
                   else if (number card1) < (number card1') then -1
                   else if (number card1) > (number card1') then 1
                   else 0)
                   | _ -> 1)
  | Fullhouse (card3, card2) ->
                  (match secondhand with
                   | Straightflush _ -> -1
                   | Fourofkind _ -> -1
                   | Fullhouse (card3', card2') ->
                   (if (number card3) > (number card3') then 1
                   else if (number card3) < (number card3') then -1
                   else if (number card2) > (number card2') then 1
                   else if (number card2) < (number card2') then -1
                   else 0)
                   | _ -> 1)

  | Flush (s, cardlst) ->
                  (match secondhand with
                   | Straightflush _ -> -1
                   | Fourofkind _ -> -1
                   | Fullhouse _ -> -1
                   | Flush (s', cardlst') -> cardlistcompare cardlst cardlst'
                   | _ -> 1)
  | Straight highcard ->
                (match secondhand with
                   | Straight highcard'->
                   (if (number highcard) > (number highcard') then 1
                   else if (number highcard) < (number highcard') then -1
                   else 0)
                   | Threeofkind _ -> 1
                   | Twopair _ -> 1
                   | Onepair _ -> 1
                   | Highcard _ -> 1
                   | _ -> -1)
  | Threeofkind (triple, kicker) ->
                (match secondhand with
                 | Twopair _ -> 1
                 | Onepair _ -> 1
                 | Highcard _ -> 1
                 | Threeofkind (triple', kicker') ->
                 (if (number triple) > (number triple') then 1
                 else if (number triple) < (number triple') then -1
                 else cardlistcompare kicker kicker')
                 | _ -> -1)
  | Twopair (pair1, pair2, kicker) ->
                (match secondhand with
                 | Onepair _ -> 1
                 | Highcard _ -> 1
                 | Twopair (pair1', pair2', kicker') ->
                 (    if (number pair1) > (number pair1') then 1
                 else if (number pair1) < (number pair1') then -1
                 else if (number pair2) > (number pair2') then 1
                 else if (number pair2) < (number pair2') then -1
                 else if (number kicker) > (number kicker') then 1
                 else if (number kicker) < (number kicker') then -1
                 else 0)
                 | _ -> -1)
  | Onepair (pair, kicker) ->
                (match secondhand with
                 | Highcard _ -> 1
                 | Onepair (pair', kicker') ->
                 (     if (number pair) > (number pair') then 1
                  else if (number pair) < (number pair') then -1
                  else cardlistcompare kicker kicker')
                 | _ -> -1)
  | Highcard cardlst ->
                (match secondhand with
                   | Highcard cardlst' -> cardlistcompare cardlst cardlst'
                   | _ -> -1)

(*Returns a list of the cards sorted from greatest to least*)
let card_sort clist =
  let compare_helper card1 card2 =
    let p1 = card_to_points card1 in
    let p2 = card_to_points card2 in
    if p1=p2 then 0
    else if p1 > p2 then (-1) else 1 in
  List.sort compare_helper clist


(*Returns true if a a 5 card hand is a flush*)
let is_flush (clist:card list) : bool =
  let rec helper cards s =
    match cards with
      | [] -> true
      | h::t -> if h.suit = s then true && helper t s else false in
  helper clist ((List.hd clist).suit)

(*Returns true if a a 5 card hand is a straight*)
let is_straight (clist:card list) : bool =
  let sorted_ints = cardlist_to_points (card_sort clist) in
  let rec helper lst =
    match lst with
      | h::m::t -> if h-m = 1 then helper (m::t) else false
      | _ -> true
  in helper sorted_ints


(*If the 5 card list is a straight flush, return Some Straightflush
  else return None*)
let make_straight_flush (clist:card list) : hand option  =
  if (is_flush clist) && (is_straight clist) then
    let first = (List.hd (card_sort clist)) in
    Some (Straightflush(first.suit, first))
  else None


(*If the 5 card list is a four of a kind, return Some Fourofkind
  else return None*)
let make_four_kind (clist:card list) : hand option  =
  let sorted = card_sort clist in
  if ((List.nth sorted 0).value = (List.nth sorted 1).value &&
      (List.nth sorted 1).value = (List.nth sorted 2).value &&
      (List.nth sorted 2).value = (List.nth sorted 3).value)
  then Some (Fourofkind ((List.hd sorted),(List.nth sorted 4)))
  else
     if ((List.nth sorted 1).value = (List.nth sorted 2).value &&
         (List.nth sorted 2).value = (List.nth sorted 3).value &&
         (List.nth sorted 3).value = (List.nth sorted 4).value)
     then Some (Fourofkind ((List.nth sorted 4), (List.hd sorted)))
     else None

(*If the 5 card list is a full house, return Some Fullhouse
  else return None*)
let make_full_house (clist:card list) : hand option  =
  let sorted = card_sort clist in
  if ((List.nth sorted 0).value = (List.nth sorted 1).value &&
      (List.nth sorted 1).value = (List.nth sorted 2).value &&
      (List.nth sorted 3).value = (List.nth sorted 4).value)
  then Some (Fullhouse ((List.nth sorted 0),(List.nth sorted 3)))
  else
      if ((List.nth sorted 0).value = (List.nth sorted 1).value &&
          (List.nth sorted 2).value = (List.nth sorted 3).value &&
          (List.nth sorted 3).value = (List.nth sorted 4).value)
      then Some (Fullhouse ((List.nth sorted 2),(List.nth sorted 0)))
      else None


(*If the 5 card list is a flush, return Some Flush
  else return None*)
let make_flush (clist:card list) : hand option  =
  if (is_flush clist) then
    let sorted = card_sort clist in
    Some (Flush( ((List.hd sorted).suit), sorted))
  else None

(*If the 5 card list is a straight, return Some Straight
  else return None*)
let make_straight (clist:card list) : hand option  =
  if (is_straight clist) then Some (Straight (List.hd (card_sort clist)))
  else None

(*If the 5 card list is a three of a kind, return Some Threeofkind
  else return None*)
let make_three_kind (clist:card list) : hand option  =
  let sorted = card_sort clist in
  if (List.nth sorted 0).value = (List.nth sorted 1).value &&
  (List.nth sorted 1).value = (List.nth sorted 2).value
  then Some (Threeofkind ((List.nth sorted 0),
            [(List.nth sorted 3); (List.nth sorted 4)] ))
  else
      if (List.nth sorted 1).value = (List.nth sorted 2).value &&
      (List.nth sorted 2).value = (List.nth sorted 3).value
      then Some (Threeofkind ((List.nth sorted 1),
                  [(List.nth sorted 0); (List.nth sorted 4)] ))
      else
          if (List.nth sorted 2).value = (List.nth sorted 3).value &&
          (List.nth sorted 3).value = (List.nth sorted 4).value
          then Some (Threeofkind ((List.nth sorted 2),
                  [(List.nth sorted 0); (List.nth sorted 1)] ))
          else None

(*If the 5 card list is a two pair, return Some Twopair
  else return None*)
let make_two_pair (clist:card list) : hand option  =
  let sorted = card_sort clist in
  if (List.nth sorted 0).value = (List.nth sorted 1).value &&
  (List.nth sorted 2).value = (List.nth sorted 3).value
  then Some (Twopair ((List.nth sorted 0), (List.nth sorted 2),
                     (List.nth sorted 4)))
  else
      if (List.nth sorted 1).value = (List.nth sorted 2).value &&
      (List.nth sorted 3).value = (List.nth sorted 4).value
      then Some (Twopair ((List.nth sorted 1), (List.nth sorted 3),
                     (List.nth sorted 0)))
      else
          if (List.nth sorted 0).value = (List.nth sorted 1).value &&
          (List.nth sorted 3).value = (List.nth sorted 4).value
          then Some (Twopair ((List.nth sorted 0), (List.nth sorted 3),
                     (List.nth sorted 2)))
          else None

(*If the 5 card list is a one pair, return Some Onepair
  else return None*)
let make_one_pair (clist:card list) : hand option  =
  let sorted = card_sort clist in
  if (List.nth sorted 0).value = (List.nth sorted 1).value
  then Some (Onepair ((List.nth sorted 0),
            [(List.nth sorted 2); (List.nth sorted 3); (List.nth sorted 4)] ))
  else
      if (List.nth sorted 1).value = (List.nth sorted 2).value
      then Some (Onepair ((List.nth sorted 1),
            [(List.nth sorted 0); (List.nth sorted 3); (List.nth sorted 4)] ))
      else
          if (List.nth sorted 2).value = (List.nth sorted 3).value
          then Some (Onepair ((List.nth sorted 2),
            [(List.nth sorted 0); (List.nth sorted 1); (List.nth sorted 4)] ))
          else
              if (List.nth sorted 3).value = (List.nth sorted 4).value
              then Some (Onepair ((List.nth sorted 3),
               [(List.nth sorted 0); (List.nth sorted 1); (List.nth sorted 2)]))
              else None


(*Takes a list of 5 cards and returns a corresponding hand representation*)
let cardlist_to_hand (clist: card list): hand =
  match (make_straight_flush clist) with
    | Some x -> x
    | None ->
       (match (make_four_kind clist) with
         | Some x -> x
         | None ->
            (match (make_full_house clist) with
              | Some x -> x
              | None ->
                (match (make_flush clist) with
                  | Some x -> x
                  | None ->
                    (match (make_straight clist) with
                       | Some x -> x
                       | None ->
                          (match (make_three_kind clist) with
                             | Some x -> x
                             | None ->
                                 (match (make_two_pair clist) with
                                   | Some x -> x
                                   | None ->
                                      (match (make_one_pair clist) with
                                        | Some x -> x
                                        | None -> Highcard (card_sort clist))))))))
(*=================================================================================*)

TEST_MODULE "helper_functions" = struct
open Assertions
(*conversion to number ok?*)
TEST_UNIT=
let card = {suit = Diamond; value = Two} in
let result = card_to_points card in
result === 2;
let card2 = {suit = Spade; value = Ace} in
let result2 = card_to_points card2 in
result2 === 14
(*Sorting works fine?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}] in
let sorted_hand = card_sort hand in
let result = [{suit = Heart; value = Six}; {suit = Heart; value = Five} ;
              {suit = Heart; value = Four};{suit = Heart; value = Three};
              {suit = Heart; value = Two}] in
sorted_hand === result

TEST_UNIT=
let hand = [{suit = Heart; value = Ace};   {suit = Heart; value = Ten}  ;
            {suit = Heart; value = Eight}; {suit = Heart; value = King} ;
            {suit = Heart; value = Five}] in
let sorted_hand = card_sort hand in
let result = [{suit = Heart; value = Ace}; {suit = Heart; value = King}  ;
            {suit = Heart; value = Ten};   {suit = Heart; value = Eight} ;
            {suit = Heart; value = Five}] in
sorted_hand === result
(*Does it make straights ok?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Two};   {suit = Heart; value = Three} ;
            {suit = Heart; value = Four};  {suit = Heart; value = Five}  ;
            {suit = Heart; value = Six}] in
let straight = make_straight hand in
let fourkind = make_four_kind hand in
fourkind === None;
straight === Some( Straight ({suit = Heart; value = Six}) )

TEST_UNIT=
let hand = [{suit = Club; value = Ten};   {suit = Heart; value = King} ;
            {suit = Spade; value = Queen};  {suit = Heart; value = Jack}  ;
            {suit = Diamond; value = Nine}] in
let straight = make_straight hand in
let flush = make_flush hand in
let fullhouse = make_full_house hand in
straight === Some (Straight ({suit = Heart ; value = King}) );
flush === None;
fullhouse === None

TEST_UNIT=
let hand = [{suit = Club; value = Ten};   {suit = Heart; value = King} ;
            {suit = Spade; value = Queen};  {suit = Heart; value = Jack}  ;
            {suit = Diamond; value = Eight}] in
let straight = make_straight hand in
straight === None

TEST_UNIT=
let hand = [{suit = Club; value = Four};   {suit = Heart; value = Five} ;
            {suit = Spade; value = Three};  {suit = Heart; value = Two}  ;
            {suit = Diamond; value = Ace}] in
let straight = make_straight hand in
straight === None (*make sure that Ace cannot be treated as 1. No wrap-arounds*)
(*does it make flush ok?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Ace};   {suit = Heart; value = Ten}  ;
            {suit = Heart; value = Eight}; {suit = Heart; value = King} ;
            {suit = Heart; value = Five}] in
let heart_flush = make_flush hand in
heart_flush === Some ( Flush (Heart,
         [{suit = Heart; value = Ace};   {suit = Heart; value = King}  ;
            {suit = Heart; value = Ten}; {suit = Heart; value = Eight} ;
            {suit = Heart; value = Five}]) )

TEST_UNIT=
let hand = [{suit = Club; value = Ace};   {suit = Club; value = Ten}  ;
            {suit = Club; value = Eight}; {suit = Club; value = King} ;
            {suit = Club; value = Five}] in
let club_flush = make_flush hand in
club_flush === Some ( Flush (Club,
           [{suit = Club; value = Ace};   {suit = Club; value = King}  ;
            {suit = Club; value = Ten}; {suit = Club; value = Eight} ;
            {suit = Club; value = Five}]) )

TEST_UNIT=
let hand = [{suit = Heart; value = Ace};   {suit = Club; value = Ten}  ;
            {suit = Club; value = Eight}; {suit = Club; value = King} ;
            {suit = Club; value = Five}] in
let fail = make_flush hand in
fail === None

(*does it make full_house ok?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Ten }; {suit = Club; value = Ten} ;
            {suit = Diamond; value = Ten}; {suit = Spade; value = Ace} ;
            {suit = Club; value = Ace}] in
let simple_test = make_full_house hand in
(match simple_test with
 | Some (Fullhouse (triple, pair)) -> triple.value === Ten;
                                      pair.value === Ace
 | _ -> true===false)

TEST_UNIT=
let hand = [{suit = Heart; value = Ace }; {suit = Club; value = Ace} ;
            {suit = Diamond; value = Ace}; {suit = Spade; value = Ten} ;
            {suit = Club; value = Ten}] in
let simple_test2 = make_full_house hand in
(match simple_test2 with
 | Some (Fullhouse (triple, pair)) -> triple.value === Ace;
                                      pair.value === Ten
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Heart; value = Ace }; {suit = Club; value = Ace} ;
            {suit = Diamond; value = Ace}; {suit = Spade; value = Ace} ;
            {suit = Club; value = Ten}] in
let fail = make_full_house hand in
fail === None

(*does it make 4-kind ok?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Ace }; {suit = Club; value = Ace} ;
            {suit = Diamond; value = Ace}; {suit = Spade; value = Ace} ;
            {suit = Club; value = Ten}] in
let fourkind = make_four_kind hand in
(match fourkind with
 | Some (Fourofkind (four, single)) -> four.value === Ace;
                                       single.value === Ten;
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Heart; value = Two }; {suit = Club; value = Two} ;
            {suit = Diamond; value = Two}; {suit = Spade; value = Two} ;
            {suit = Club; value = Ten}] in
let fourkind = make_four_kind hand in
(match fourkind with
 | Some (Fourofkind (four, single)) -> four.value === Two;
                                       single.value === Ten
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Heart; value = Ace }; {suit = Club; value = Ace} ;
            {suit = Diamond; value = Ace}; {suit = Spade; value = Ten} ;
            {suit = Club; value = Ten}] in
let fourkind = make_four_kind hand in
fourkind === None

(*what about straight flush?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}] in
let straight = make_straight hand in
let flush = make_flush hand in
let straightflush = make_straight_flush hand in
(match straight, flush, straightflush with
 | (Some _), (Some _), (Some (Straightflush (_,c))) -> c.value === Six
 | _, _, _ -> false === true)

TEST_UNIT=
let hand = [{suit = Heart; value = Ten}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace};   {suit = Heart; value = Queen}  ;
            {suit = Heart; value = King}] in
let straight = make_straight hand in
let flush = make_flush hand in
let straightflush = make_straight_flush hand in
(match (straight, flush, straightflush) with
 | (Some _), (Some _), (Some (Straightflush (_, c))) -> c.value === Ace
 | (_,_,_) -> false === true)

TEST_UNIT=
let hand = [{suit = Heart; value = Ten}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace}; {suit = Heart; value = Queen};
            {suit = Club; value = King}] in
let fail = make_straight_flush hand in
fail === None

TEST_UNIT=
let hand = [{suit = Heart; value = Nine}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace};  {suit = Heart; value = Queen};
            {suit = Heart; value = King}] in
let fail = make_straight_flush hand in
fail === None

(*three kind*)
TEST_UNIT=
let hand = [{suit = Diamond; value = Nine}; {suit = Heart; value = Eight} ;
            {suit = Club; value = Nine};  {suit = Spade; value = Nine};
            {suit = Spade; value = Eight}] in
let three = make_three_kind hand in
(match three with
 | Some (Threeofkind(triple, kickers)) -> triple.value === Nine;
                                    (cardlist_to_points kickers) === [8;8]
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Diamond; value = Ten}; {suit = Heart; value = Nine} ;
            {suit = Club; value = Nine};  {suit = Spade; value = Nine};
            {suit = Spade; value = Eight}] in
let three = make_three_kind hand in
(match three with
 | Some (Threeofkind (triple, kickers)) -> triple.value === Nine;
                                    (cardlist_to_points kickers) === [10;8]
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Diamond; value = Two}; {suit = Heart; value = Three} ;
            {suit = Club; value = Four};  {suit = Spade; value = Eight};
            {suit = Spade; value = Eight}] in
let three = make_three_kind hand in
three === None
(*two pair*)
TEST_UNIT=
let hand = [{suit = Diamond; value = Nine}; {suit = Heart; value = Eight} ;
            {suit = Club; value = Nine};  {suit = Spade; value = Ten};
            {suit = Spade; value = Eight}] in
let double = make_two_pair hand in
(match double with
 | Some (Twopair (pair1,pair2,kicker)) -> pair1.value === Nine;
                                          pair2.value === Eight;
                                          kicker === {suit = Spade; value = Ten}
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Diamond; value = Ace}; {suit = Heart; value = Ace} ;
            {suit = Club; value = Ace};  {suit = Spade; value = Ace};
            {suit = Spade; value = Nine}] in
let double = make_two_pair hand in
(match double with
 | Some (Twopair (pair1, pair2, kicker)) -> pair1.value === Ace;
                                            pair2.value === Ace;
                                            kicker === {suit = Spade; value = Nine}
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Diamond; value = Ace}; {suit = Heart; value = Ace} ;
            {suit = Club; value = Four};  {suit = Spade; value = Five};
            {suit = Spade; value = Nine}] in
let fail = make_two_pair hand in
fail === None
TEST_UNIT=
let hand = [{suit = Diamond; value = Ace}; {suit = Heart; value = Ace} ;
            {suit = Club; value = Four};  {suit = Spade; value = Four};
            {suit = Spade; value = Ace}] in
let double = make_two_pair hand in
(match double with
 | Some (Twopair (pair1, pair2, kicker)) -> pair1.value === Ace;
                                            pair2.value === Four;
                                            kicker.value === Ace
 | _ -> true === false)
(*one pair*)
TEST_UNIT=
let hand = [{suit = Diamond; value = Ace}; {suit = Heart; value = Ace} ;
            {suit = Club; value = Four};  {suit = Spade; value = Five};
            {suit = Spade; value = Nine}] in
let one = make_one_pair hand in
(match one with
 | Some (Onepair (c, clist)) -> c.value === Ace;
                                clist ===
                                [{suit = Spade; value = Nine};
                                {suit = Spade; value = Five};
                                {suit = Club; value = Four}]
 | _ -> true === false)

TEST_UNIT=
let hand = [{suit = Diamond; value = Ace}; {suit = Heart; value = Three} ;
           {suit = Club; value = Four};  {suit = Spade; value = Five};
           {suit = Spade; value = Nine}] in
let one = make_one_pair hand in
one === None
end