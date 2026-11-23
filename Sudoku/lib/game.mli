(** The state of one game as the player sees it: which cells were given,
    what the player has typed, and which cell is selected.

    Values of type [t] are immutable. Every function that "changes" a game
    returns a new one, so the UI can keep old states if it wants (for
    example, for an undo feature later). Rows and columns are numbered 0
    through 8, starting from the top-left cell. *)

type t

(** What a single cell shows. *)
type cell =
  | Given of int  (** A clue from the generated puzzle. Cannot be changed. *)
  | Correct of int  (** Typed by the player and matches the solution. *)
  | Wrong of int  (** Typed by the player and does not match the solution. *)
  | Empty  (** Not filled in yet. *)

(** [create p] is a new game for puzzle [p] with no player entries and the
    top-left cell selected. It reads [p] but does not modify it. *)
val create : Puzzles.t -> t

(** [cell g row col] is what the cell at ([row], [col]) shows in [g].
    Requires: [row] and [col] are in 0..8. *)
val cell : t -> int -> int -> cell

(** [cursor g] is the ([row], [col]) of the selected cell. *)
val cursor : t -> int * int

(** [select g row col] is [g] with the cell at ([row], [col]) selected.
    Coordinates outside 0..8 are clamped to the nearest edge. *)
val select : t -> int -> int -> t

(** [move g dr dc] is [g] with the selection moved by [dr] rows and [dc]
    columns, stopping at the edges of the grid. *)
val move : t -> int -> int -> t

(** [enter g n] is [g] with [n] typed into the selected cell. It is [g]
    unchanged if the selected cell is a given or [n] is not in 1..9. *)
val enter : t -> int -> t

(** [clear g] is [g] with the player's entry in the selected cell removed.
    It is [g] unchanged if the selected cell is a given. *)
val clear : t -> t

(** [wrong_count g] is the number of cells currently showing [Wrong]. *)
val wrong_count : t -> int

(** [is_complete g] is [true] if every cell is [Given] or [Correct], that
    is, the player has solved the puzzle. *)
val is_complete : t -> bool
