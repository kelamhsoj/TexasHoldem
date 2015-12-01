open Random

type suits =
  | Heart
  | Spade
  | Club
  | Diamond

type values =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

type card = {suit: suits; value: values}

type deck = card list ref

let suit_to_string = function
  | Heart -> "Hearts"
  | Spade -> "Spades"
  | Club -> "Clubs"
  | Diamond -> "Diamonds"

let to_suit = function
  | 1 -> Heart
  | 2 -> Spade
  | 3 -> Club
  | 4 -> Diamond
  | _ -> failwith "error"

let value_to_string = function
  | Ace -> "Ace"
  | King -> "King"
  | Queen -> "Queen"
  | Jack -> "Jack"
  | Ten -> "10"
  | Nine -> "9"
  | Eight -> "8"
  | Seven -> "7"
  | Six -> "6"
  | Five -> "5"
  | Four -> "4"
  | Three -> "3"
  | Two -> "2"

let to_value = function
  | 1 -> Ace
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | 10 -> Ten
  | 11 -> Jack
  | 12 -> Queen
  | 13 -> King
  | _ -> failwith "error"

let remove d (i: int) =
  let el = List.nth d i in
  List.filter ((<>) el) d


let rec shuffle (d: deck) =
  let len = List.length !d in
  let i = Random.int len in
  let ret = List.nth !d i in
  let newd = ref (remove !d i) in
  ref (ret::!(shuffle newd))

let rec pop d i =
  let len = List.length !d in
  if i > len then failwith "Not enough cards remaining"
  else match i with
  | 0 -> []
  | _ -> let ret = (List.hd !d) in
         d := (List.tl !d);
         ret::(pop d (i-1))

let newdeck () =
  let result = ref [] in
  for x = 1 to 13 do
    for y = 1 to 4 do
      let value = to_value x in
      let suit = to_suit y in
      let cardi = {suit=suit; value=value} in
      result := cardi::!result
    done
  done;
  result


let printcard p =
  print_string ((value_to_string p.value) ^ " of " ^ (suit_to_string p.suit))