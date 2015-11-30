open Cardcomp
open Assertions
open Deck

(*TESTING OF HELPER FUNCTIONS, THE ONLY ONES THAT TAKE 5 CARDS OCCURS
 * IN CARDCOMP SINCE HELPER FUNCTIONS ARE HIDDEN *)
(*conversion to number ok?*)
TEST_MODULE "testing the main functions" =
struct
open Cardcomp
open Assertions
open Deck

TEST_UNIT=
(*tRy with basic 5, multiple different hands*)
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}] in
let straightflush = best_hand hand in
straightflush === Straightflush(Heart, Six)

TEST_UNIT=
(*try with 6*)
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}; {suit= Heart; value = Seven}] in
let straightflush = best_hand hand in
straightflush === Straightflush(Heart,Seven)

(*try with 7*)
TEST_UNIT=
let hand = [{suit = Heart; value = Three}; {suit = Heart; value = Four} ;
            {suit = Heart; value = Six};   {suit = Heart; value = Two}  ;
            {suit = Heart; value = Five}; {suit= Heart; value = Seven}  ;
            {suit = Club; value = Eight}] in
let strightflush = best_hand hand in
straightflush === Straightflush(Heart,Seven)

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
(*comparison of different hands goes here.*)
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
TEST_UNIT=
end
