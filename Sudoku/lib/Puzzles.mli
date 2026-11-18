(** Type t represents a sudoku puzzle and its solution *)
type t

(** Generates a new sudoku puzzle 
NOT REAL PUZZLE LOGIC*)
val generate : unit -> t

(** Returns the current unsolved puzzle as a list of lists of integers *)
val get_puzzle : t -> int option list list

(** Checks if a user input sudoku puzzle is valid *)
val check : int option list list -> t -> bool

(** Solves a sudoku puzzle *)