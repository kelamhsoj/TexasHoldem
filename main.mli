(*Holds all the information for the current game including: pot, scores, cards
on the table *)
type gamestate


(*Main game engine that takes in a gamestate and prints to the user/
requests user input*)
val engine: gamestate -> unit