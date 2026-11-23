type cell =
  | Given of int
  | Correct of int
  | Wrong of int
  | Empty

(* AF: [{ sudoku; givens; entries; cursor }] is a game of [sudoku] where
   [givens.(r).(c) = Some n] means cell (r, c) is the clue [n],
   [entries.(r).(c) = Some n] means the player typed [n] there, and [cursor]
   is the selected (row, col).
   RI: [givens] and [entries] are 9x9; no cell is [Some] in both; every
   [Some n] has [n] in 1..9; both components of [cursor] are in 0..8.
   [entries] is never mutated after a [t] is built, so values can share it. *)
type t = {
  sudoku : Puzzles.t;
  givens : int option array array;
  entries : int option array array;
  cursor : int * int;
}

let clamp i = max 0 (min 8 i)

let create sudoku =
  let givens =
    Puzzles.get_puzzle sudoku
    |> List.map Array.of_list |> Array.of_list
  in
  { sudoku; givens; entries = Array.make_matrix 9 9 None; cursor = (0, 0) }

let cell g row col =
  match (g.givens.(row).(col), g.entries.(row).(col)) with
  | Some n, _ -> Given n
  | None, Some n ->
      if Puzzles.validate n row col g.sudoku then Correct n else Wrong n
  | None, None -> Empty

let cursor g = g.cursor
let select g row col = { g with cursor = (clamp row, clamp col) }

let move g dr dc =
  let r, c = g.cursor in
  select g (r + dr) (c + dc)

(** [set_entry g v] is [g] with the selected cell's entry replaced by [v],
    or [g] itself if that cell is a given. *)
let set_entry g v =
  let r, c = g.cursor in
  if g.givens.(r).(c) <> None then g
  else
    let entries = Array.map Array.copy g.entries in
    entries.(r).(c) <- v;
    { g with entries }

let enter g n = if n < 1 || n > 9 then g else set_entry g (Some n)
let clear g = set_entry g None

(** [count g p] is the number of cells of [g] whose [cell] satisfies [p]. *)
let count g p =
  let total = ref 0 in
  for r = 0 to 8 do
    for c = 0 to 8 do
      if p (cell g r c) then incr total
    done
  done;
  !total

let wrong_count g = count g (function Wrong _ -> true | _ -> false)

let is_complete g =
  count g (function Given _ | Correct _ -> true | _ -> false) = 81
