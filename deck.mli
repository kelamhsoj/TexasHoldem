(*Represents clubs, hearts, diamonds, spades *)
type suits =
  | Heart
  | Spade
  | Club
  | Diamond

(*Represents ace, king, queen, jack, 10, 9, 8, 7, 6, 5, 4, 3, 2 *)
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

(*Represents a card with a suit and a value*)
type card = {suit: suits; value: values}

(*Represents a list of cards*)
type deck = card list ref

(*Randomly shuffles the element of the deck*)
val shuffle: deck -> deck

(*Returns the first n elements of the deck*)
val pop: deck -> int -> card list

(*Returns a new shuffled deck*)
val newdeck: unit -> deck
