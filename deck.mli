(*Represents a card with a suit and a value*)
type card

(*Represents a list of cards*)
type deck

(*Represents clubs, hearts, diamonds, spades *)
type suit

(*Represents ace, king, queen, jack, 10, 9, 8, 7, 6, 5, 4, 3, 2 *)
type value

(*Randomly shuffles the element of the deck*)
val shuffle: deck -> deck

(*Returns the first n elements of the deck*)
val pop: deck -> int -> card list

