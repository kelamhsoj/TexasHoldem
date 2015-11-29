open Cardcomp
open Assertions
open Deck

(*TESTING HELPER FUNCTIONS. THESE ONLY TAKE 5 CARDS*)
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
            {suit = Heart; value = Five}]
(*Does it make straights ok?*)
TEST_UNIT=
let hand = [{suit = Heart; value = Two};   {suit = Heart; value = Three} ;
            {suit = Heart; value = Four};  {suit = Heart; value = Five}  ;
            {suit = Heart; value = Six}] in
let straight = make_straight hand in
let fourkind = make_four_kind hand in
fourkind === None; 
straight === Some( Straight ({suit = Heart; value = Six}) );

TEST_UNIT=
let hand = [{suit = Club; value = Ten};   {suit = Heart; value = King} ;
            {suit = Spade; value = Queen};  {suit = Heart; value = Jack}  ;
            {suit = Diamond; value = Nine}] in
let straight = make_straight hand in
let flush = make_flush hand in
let fullhouse = make_full_house in
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
 | None -> true===false)

TEST_UNIT=
let hand = [{suit = Heart; value = Ace }; {suit = Club; value = Ace} ; 
            {suit = Diamond; value = Ace}; {suit = Spade; value = Ten} ; 
            {suit = Club; value = Ten}] in
let simple_test2 = make_full_house hand in
(match simple_test2 with
 | Some (Fullhouse (triple, pair)) -> triple.value === Ace;
                                      pair.value === Ten
 | None -> true === false)

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
 | None -> true === false)

TEST_UNIT=  
let hand = [{suit = Heart; value = Two }; {suit = Club; value = Two} ; 
            {suit = Diamond; value = Two}; {suit = Spade; value = Two} ; 
            {suit = Club; value = Ten}] in
let fourkind = make_four_kind hand in
(match fourkind with
 | Some (Fourofkind (four, single)) -> four.value === Two;
                                       single.value === Ten
 | None -> true === false)

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
(match (straight, flush, straightflush) with
 | (Some _, Some _, Some (_,c)) -> c.value === Six
 | _, _, _ -> false === true)

TEST_UNIT=
let hand = [{suit = Heart; value = Ten}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace};   {suit = Heart; value = Queen}  ;
            {suit = Heart; value = King}] in
let straight = make_straight hand in
let flush = make_flush hand in 
let straightflush = make_straight_flush hand in
(match (straight, flush, straightflush) with
 | (Some _, Some _, Some (_, c)) -> c.value === Ace
 | (_,_,_) -> false === true)

TEST_UNIT=
let hand = [{suit = Heart; value = Ten}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace};   {suit = Heart; value = Queen}  ;
            {suit = Club; value = King}] in
let fail = make_straight_flush hand in
fail === None

TEST_UNIT= [{suit = Heart; value = Nine}; {suit = Heart; value = Jack} ;
            {suit = Heart; value = Ace};   {suit = Heart; value = Queen}  ;
            {suit = Heart; value = King}] in
let fail = make_straight_flush hand in
fail === None
(*
(*two pair*)
TEST_UNIT=

TEST_UNIT=
TEST_UNIT=
(*one pair*)
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
(*high card*)
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=




TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
*)