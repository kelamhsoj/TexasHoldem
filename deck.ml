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

let to_suit = function
  | 1 -> Heart
  | 2 -> Spade
  | 3 -> Club
  | 4 -> Diamond
  | _ -> failwith "error"

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
