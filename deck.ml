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

type deck = card list



let shuffle d = failwith "TODO"

let pop d i = failwith "TODO"

let newdeck () = failwith "TODO"