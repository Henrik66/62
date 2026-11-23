(** Backtracking Sudoku solver.

    A puzzle is a 9x9 grid of rows. [Some n] is a filled cell holding [n]
    (1 through 9), and [None] is an empty cell. No function in this module
    modifies the grid it is given. *)

type puzzle = int option array array

(** [solve p] is [Some s], where [s] is a completed grid that fills in every
    empty cell of [p] and keeps every filled cell unchanged, or [None] if [p]
    has no solution. If [p] has more than one solution, [s] is one of them.
    Requires: [p] is 9x9. *)
val solve : puzzle -> int array array option

(** [count_solutions p] is the number of solutions of [p], capped at [2]. It
    is [0] if [p] has no solution (including when its filled cells already
    break a rule), [1] if it has exactly one, and [2] if it has two or more.
    Requires: [p] is 9x9. *)
val count_solutions : puzzle -> int
