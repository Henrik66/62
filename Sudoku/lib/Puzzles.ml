(*Representation of sudoku puzzles and generators*)
let () = Random.self_init ()
type t = { puzzle : int option array array; solution : int array array }

(* Representation invariant: 
   - The puzzle is a 9x9 grid where each cell can be None (empty) or Some int (filled).
   - The solution is a 9x9 grid of integers from 1 to 9. Each row, column, and 3x3 subgrid 
   must contain all numbers from 1 to 9 without repetition.
   *)
(** [rep_ok sudoku] checks whether [sudoku]'s solution is a valid completed
    9-by-9 Sudoku grid: every row, column, and 3-by-3 box has no duplicates
    and contains values in the range 1 through 9. *)
let rep_ok (sudoku : t) : bool =
  let check_row row =
    let seen = Array.make 10 false in
    Array.for_all (fun num ->
      if num >= 1 && num <= 9 then
        if seen.(num) then false else (seen.(num) <- true; true)
      else false
    ) row
  in
  let check_col col =
    let seen = Array.make 10 false in
    Array.for_all (fun row ->
      let num = row.(col) 
    in 
    if num >= 1 && num <= 9 then
          if seen.(num) then false else (seen.(num) <- true; true)
    else false) sudoku.solution
  in
  let check_subgrid start_row start_col =
    let seen = Array.make 10 false in
    for i = start_row to start_row + 2 do
      for j = start_col to start_col + 2 do
        match sudoku.solution.(i).(j) with
        | num when num >= 1 && num <= 9 ->
            if seen.(num) then () else (seen.(num) <- true)
        | _ -> ()
      done
    done;
    Array.for_all (fun x -> x || x = false) seen
  in
  let rows_valid = Array.for_all check_row sudoku.solution in
  let cols_valid = Array.for_all check_col (Array.init 9 (fun i -> i)) in
  let subgrids_valid =
    List.for_all (fun (r, c) -> check_subgrid r c)
      [(0,0); (0,3); (0,6); (3,0); (3,3); (3,6); (6,0); (6,3); (6,6)]
  in
  rows_valid && cols_valid && subgrids_valid

(** [randomize lst] returns the elements of [lst] in a randomly shuffled
    order. *)
let randomize lst =
  let row = Array.of_list lst in 
  for i = 8 downto 1 do
    let j = Random.int (i + 1) in
    let temp = row.(i) in
    row.(i) <- row.(j);
    row.(j) <- temp;
  done;
  Array.to_list row
(** [is_valid grid row col num] is [true] if [num] can be placed at
    ([row], [col]) without repeating a value in that row, column, or 3-by-3
    box. [grid] uses [0] to represent an empty cell. *)
let is_valid grid row col num =
  let box_row = (row / 3) * 3 in
  let box_col = (col / 3) * 3 in
  let rec check i = if i < 9 then
      if grid.(row).(i) = num || grid.(i).(col) =  num ||
         grid.(box_row + (i / 3)).(box_col + (i mod 3)) =  num
      then false else check (i + 1)
    else true
  in check 0

(** [fill_grid grid row col] fills [grid] with a valid completed Sudoku,
    starting at ([row], [col]). It returns [true] when the grid is filled
    and [false] if the current choices cannot produce a solution. The grid is
    updated in place. *)
let rec fill_grid grid row col =
  if row = 9 then true
  else
    let next_row, next_col = if col = 8 then (row + 1, 0) else (row, col + 1) in
    let candidates = randomize [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let rec try_candidates = function
    | [] -> false
    | n :: rest ->
      if is_valid grid row col n then begin
            grid.(row).(col) <- n;
            if fill_grid grid next_row next_col then true
            else begin
              grid.(row).(col) <- 0;
              try_candidates rest
            end
             end else
            try_candidates rest
    in
    try_candidates candidates

(** [generate ()] creates a complete valid Sudoku solution, then removes clues
    in a randomized order. A clue is kept removed only when the resulting
    puzzle still has exactly one solution. *)
let generate () : t =
  let solution = Array.make_matrix 9 9 0 in
  ignore (fill_grid solution 0 0);
(* Start with every cell of the completed solution as a clue. *)
let puzzle = Array.map Array.copy solution in
let puzzle = Array.map (Array.map (fun n -> Some n)) puzzle in

(* Shuffle the cell indices so each generated puzzle tries removals in a
   different order. *)
let cells = List.init 81 Fun.id |> Array.of_list in
for i = 80 downto 1 do
  let j = Random.int (i + 1) in
  let temp = cells.(i) in
  cells.(i) <- cells.(j);
  cells.(j) <- temp
done;

(* Try removing each clue. Restore it whenever removal makes the puzzle
   unsolvable or gives it more than one solution. *)
Array.iter (fun cell ->
  let row = cell / 9 in
  let col = cell mod 9 in
  let saved = puzzle.(row).(col) in
  puzzle.(row).(col) <- None;
  if Solver.count_solutions puzzle <> 1 then
    puzzle.(row).(col) <- saved
) cells;

{ puzzle; solution }

(** [get_puzzle sudoku] returns the puzzle as a list of rows. Empty cells are
    represented by [None], and clue values by [Some n]. *)
let get_puzzle (sudoku : t) : int option list list =
    Array.to_list (Array.map Array.to_list sudoku.puzzle)

(** [get_solution sudoku] returns the completed solution as a list of rows. *)
    let get_solution (sudoku : t) : int list list =
    Array.to_list (Array.map Array.to_list sudoku.solution)


(** [check num row col gen_solution] checks whether [num] is the solution
    value at ([row], [col]) in [gen_solution]. If correct, it records [num]
    in the puzzle and returns [true]; otherwise it returns [false]. *)
  let check num row col (gen_solution : t) =
    let user_array = gen_solution.puzzle in
    let solution_array = gen_solution.solution in
    if solution_array.(row).(col) = num then
      (user_array.(row).(col) <- Some num; true)
    else
      false
    


