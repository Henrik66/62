type puzzle = int option array array

(* Internally the solver works on an [int array array] where [0] means an
   empty cell. This makes "is this cell empty?" and "write a number" cheap. *)

(** [to_ints p] is a fresh copy of [p] with [None] replaced by [0]. *)
let to_ints (p : puzzle) : int array array =
  Array.map (Array.map (function Some n -> n | None -> 0)) p

(** [can_place grid row col num] is [true] if [num] does not already appear
    in row [row], column [col], or the 3x3 box containing ([row], [col]).
    The cell ([row], [col]) itself is ignored. *)
let can_place grid row col num =
  let box_row = row / 3 * 3 in
  let box_col = col / 3 * 3 in
  let rec check i =
    if i = 9 then true
    else
      let r = box_row + (i / 3) and c = box_col + (i mod 3) in
      if
        (i <> col && grid.(row).(i) = num)
        || (i <> row && grid.(i).(col) = num)
        || ((r, c) <> (row, col) && grid.(r).(c) = num)
      then false
      else check (i + 1)
  in
  check 0

(** [givens_ok grid] is [true] if no filled cell of [grid] breaks a rule,
    that is, every filled cell holds 1 through 9 and no two filled cells in
    the same row, column, or box share a number. Without this check the
    search would happily "solve" a puzzle whose clues already conflict. *)
let givens_ok grid =
  let ok = ref true in
  for r = 0 to 8 do
    for c = 0 to 8 do
      let n = grid.(r).(c) in
      if n <> 0 && (n < 1 || n > 9 || not (can_place grid r c n)) then
        ok := false
    done
  done;
  !ok

(** [candidates grid row col] is the list of numbers that could go in the
    empty cell ([row], [col]) right now. *)
let candidates grid row col =
  List.filter (can_place grid row col) [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

(** [best_cell grid] picks the empty cell with the fewest candidates (the
    "most constrained" cell) and returns [Some (row, col, cands)], or [None]
    if the grid has no empty cells. Trying the hardest cell first keeps the
    search small: a cell with one candidate is forced, and a cell with zero
    tells us immediately that this branch is a dead end. *)
let best_cell grid =
  let best = ref None in
  let best_size = ref 10 in
  (try
     for r = 0 to 8 do
       for c = 0 to 8 do
         if grid.(r).(c) = 0 then begin
           let cands = candidates grid r c in
           let size = List.length cands in
           if size < !best_size then begin
             best := Some (r, c, cands);
             best_size := size;
             (* Can't do better than zero or one choice, so stop looking. *)
             if size <= 1 then raise Exit
           end
         end
       done
     done
   with Exit -> ());
  !best

(** [search grid limit on_solution] runs a backtracking search over [grid],
    calling [on_solution grid] each time the grid becomes full. It stops
    after finding [limit] solutions and returns how many it found. [grid] is
    restored to its starting state when [search] returns. *)
let search grid limit on_solution =
  let found = ref 0 in
  let rec go () =
    if !found < limit then
      match best_cell grid with
      | None ->
          incr found;
          on_solution grid
      | Some (r, c, cands) ->
          List.iter
            (fun n ->
              if !found < limit then begin
                grid.(r).(c) <- n;
                go ();
                grid.(r).(c) <- 0
              end)
            cands
  in
  go ();
  !found

let solve (p : puzzle) : int array array option =
  let grid = to_ints p in
  if not (givens_ok grid) then None
  else
    let answer = ref None in
    let save g = answer := Some (Array.map Array.copy g) in
    ignore (search grid 1 save);
    !answer

let count_solutions (p : puzzle) : int =
  let grid = to_ints p in
  if not (givens_ok grid) then 0 else search grid 2 (fun _ -> ())
