(** Type t represents a sudoku puzzle and its solution *)
type t

(** Generates a new sudoku puzzle *)
val generate : unit -> t

(** Returns the current unsolved puzzle as a list of lists of integers *)
val get_puzzle : t -> int option list list

(** Checks if a user input sudoku puzzle is valid
    alters the puzzle in place and returns true if valid
    input is number, row, column *)
val check : int -> int -> int -> t -> bool

(** Returns the solution of the sudoku puzzle as a list of lists of integers *)
val get_solution : t -> int list list

(** Checks if the representation of the sudoku puzzle is valid *)
val rep_ok : t -> bool
