(*TESTING OF HELPER FUNCTIONS, THE ONLY ONES THAT TAKE 5 CARDS OCCURS
 * IN CARDCOMP SINCE HELPER FUNCTIONS ARE HIDDEN *)
(*conversion to number ok?*)
TEST_MODULE "testing the main functions" =
struct
open Cardcomp
open Assertions
open Deck

TEST_UNIT=
(*Try with basic 5, multiple different hands*)
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}] in
let straightflush = best_hand hand in
straightflush === Straightflush(Heart, {suit = Heart; value = Six})

TEST_UNIT=
(*try with 6*)
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}; {suit= Heart; value = Seven}] in
let straightflush = best_hand hand in
straightflush === Straightflush(Heart, {suit= Heart; value = Seven})

(*try with 7*)
TEST_UNIT=
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}; {suit= Heart; value = Seven}  ;
            {suit = Club; value = Eight}] in
let straightflush = best_hand hand in
straightflush === Straightflush(Heart,{suit= Heart; value = Seven})
(* 4 kind*)
TEST_UNIT=
let hand = [{suit = Heart; value = Ace}; {suit = Spade; value = Ace} ;
            {suit = Club; value = Ace};   {suit = Diamond; value = Ace}  ;
            {suit = Heart; value = King}; {suit= Heart; value = Queen}  ;
            {suit = Club; value = King}] in
let fourkind = best_hand hand in
match fourkind with
 | Fourofkind (a,k) -> a.value === Ace;
                       k.value === King
 | _ -> true === false
(* full house *)
TEST_UNIT=
let hand = [{suit = Heart; value = Ace}; {suit = Spade; value = Ace} ;
            {suit = Club; value =  Queen};   {suit = Diamond; value = Ace}  ;
            {suit = Heart; value = King}; {suit= Heart; value = Queen}  ;
            {suit = Club; value = King}] in
let fullhouse = best_hand hand in
match fullhouse with
 | Fullhouse (triple, pair) -> triple.value === Ace;
                               pair.value === King
 | _ -> true === false

TEST_UNIT=
let hand = [{suit = Heart; value = Queen}; {suit = Spade; value = Five} ;
            {suit = Club;  value = Queen}; {suit = Diamond; value = Ace}  ;
            {suit = Heart; value = King};  {suit = Diamond; value = Queen}  ;
            {suit = Club;  value = King}] in
let fullhouse = best_hand hand in
match fullhouse with
 | Fullhouse (triple, pair) -> triple.value === Queen;
                               pair.value === King
 | _ -> true === false
(* flush *)
TEST_UNIT=
let hand = [{suit = Heart; value = Eight}; {suit = Heart; value = Seven};
            {suit = Heart; value = Ace}; {suit = Heart; value = Two};
            {suit = Heart; value = Ten}; {suit = Heart; value = Four};
            {suit = Heart; value = King}] in
let flush = best_hand hand in
match flush with
 | Flush (s,lst) -> s === Heart;
                    lst === [{suit = Heart; value = Ace};
                             {suit = Heart; value = King};
                             {suit = Heart; value = Ten};
                             {suit = Heart; value = Eight};
                             {suit = Heart; value = Seven}]
 | _ -> true === false

TEST_UNIT=
let hand = [{suit = Heart; value = Jack}; {suit = Heart; value = Eight};
            {suit = Heart; value = Five}; {suit = Heart; value = Six};
            {suit = Heart; value = Three}; {suit = Heart; value = Four};
            {suit = Spade; value = Two}] in
let flush = best_hand hand in
match flush with
 | Flush (s,lst) -> s === Heart;
                    lst === [{suit = Heart; value = Jack};
                             {suit = Heart; value = Eight};
                             {suit = Heart; value = Six};
                             {suit = Heart; value = Five};
                             {suit = Heart; value = Four}]
 | _ -> true === false
(* straight *)
TEST_UNIT=
let hand = [{suit = Spade; value = Jack}; {suit = Heart; value = Eight};
            {suit = Diamond; value = Five}; {suit = Heart; value = Six};
            {suit = Club; value = Three}; {suit = Heart; value = Four};
            {suit = Spade; value = Two}] in
let straight = best_hand hand in
match straight with
 | Straight c -> c.suit  === Heart;
                 c.value === Six
 | _ -> true === false

TEST_UNIT=
let hand = [{suit = Spade; value = Eight}; {suit = Heart; value = Nine};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Six};
            {suit = Club; value = Five}; {suit = Heart; value = Three};
            {suit = Spade; value = Four}] in
let straight = best_hand hand in
match straight with
 | Straight c -> c.value === Nine;
                 c.suit === Heart
 | _ -> true === false

TEST_UNIT=
let hand = [{suit = Spade; value = Eight}; {suit = Heart; value = Nine};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Six};
            {suit = Club; value = Five}; {suit = Heart; value = Five};
            {suit = Spade; value = Five}] in
let straight = best_hand hand in
match straight with
 | Straight c -> c.value === Nine;
                 c.suit === Heart
 | _ -> true === false
(* triples *)
TEST_UNIT=
let hand = [{suit = Spade; value = Ten}; {suit = Heart; value = Nine};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Six};
            {suit = Club; value = Five}; {suit = Heart; value = Five};
            {suit = Spade; value = Five}] in
let triples = best_hand hand in
match triples with
 | Threeofkind(triple, kicker) -> triple.value === Five;
                                  kicker === [{suit = Spade; value = Ten};
                                             {suit = Heart; value = Nine}]
 | _ -> true === false
(* Double pair *)
TEST_UNIT=
let hand = [{suit = Spade; value = Ace}; {suit = Heart; value = Four};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Three};
            {suit = Club; value = Five}; {suit = Heart; value = Three};
            {suit = Spade; value = Five}] in
let doublepairs = best_hand hand in
match doublepairs with
 | Twopair (pair1,pair2,kicker) -> pair1.value === Five;
                                   pair2.value === Three;
                                   kicker === {suit = Spade; value = Ace}
 | _ -> true === false
(* pair *)
TEST_UNIT=
let hand = [{suit = Spade; value = Ace}; {suit = Heart; value = Four};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Three};
            {suit = Club; value = Five}; {suit = Heart; value = Ten};
            {suit = Spade; value = Five}] in
let pair = best_hand hand in
match pair with
 | Onepair (pair1, kicker) -> pair1.value === Five;
                              kicker === [{suit = Spade; value = Ace};
                                          {suit = Heart; value = Ten};
                                          {suit = Diamond; value = Seven}]
 | _ -> true === false
TEST_UNIT=
let hand = [{suit = Spade; value = Ace}; {suit = Heart; value = Four};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Three};
            {suit = Club; value = Five}; {suit = Heart; value = Ten};
            {suit = Heart; value = Ace}] in
let pair = best_hand hand in
match pair with
 | Onepair (pair1, kicker) -> pair1.value === Ace;
                              kicker === [{suit = Heart; value = Ten};
                                          {suit = Diamond; value = Seven};
                                          {suit = Club; value = Five}]
 | _ -> true === false

(* high card*)
TEST_UNIT=
let hand = [{suit = Spade; value = King}; {suit = Heart; value = Four};
            {suit = Diamond; value = Seven}; {suit = Heart; value = Three};
            {suit = Club; value = Five}; {suit = Heart; value = Ten};
            {suit = Heart; value = Ace}] in
let high = best_hand hand in
match high with
| Highcard lst -> lst === [{suit = Heart; value = Ace};
                           {suit = Spade; value = King};
                           {suit = Heart; value = Ten};
                           {suit = Diamond; value = Seven};
                           {suit = Club; value = Five}]
| _ -> true === false
(*
(*comparison of different hands goes here.*)
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
*)
end
