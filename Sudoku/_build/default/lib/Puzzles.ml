(*Representation of sudoku puzzles and generators*)
let () = Random.self_init ()
type t = { puzzle : int option array array; solution : int array array }

(* Representation invariant: 
   - The puzzle is a 9x9 grid where each cell can be None (empty) or Some int (filled).
   - The solution is a 9x9 grid of integers from 1 to 9.
   - Each row, column, and 3x3 subgrid must contain all numbers from 1 to 9 without repetition.
   *)
let rep_ok (sudoku : t) : bool =
  let check_row row =
    let seen = Array.make 10 false in
    Array.for_all (function
      | None -> true
      | Some num when num >= 1 && num <= 9 ->
          if seen.(num) then false else (seen.(num) <- true; true)
      | _ -> false) row
  in
  let check_col col =
    let seen = Array.make 10 false in
    Array.for_all (fun row ->
      match row.(col) with
      | None -> true
      | Some num when num >= 1 && num <= 9 ->
          if seen.(num) then false else (seen.(num) <- true; true)
      | _ -> false) sudoku.puzzle
  in
  let check_subgrid start_row start_col =
    let seen = Array.make 10 false in
    for i = start_row to start_row + 2 do
      for j = start_col to start_col + 2 do
        match sudoku.puzzle.(i).(j) with
        | None -> ()
        | Some num when num >= 1 && num <= 9 ->
            if seen.(num) then () else (seen.(num) <- true)
        | _ -> ()
      done
    done;
    Array.for_all (fun x -> x || x = false) seen
  in
  let rows_valid = Array.for_all check_row sudoku.puzzle in
  let cols_valid = Array.for_all check_col (Array.init 9 (fun i -> i)) in
  let subgrids_valid =
    List.for_all (fun (r, c) -> check_subgrid r c)
      [(0,0); (0,3); (0,6); (3,0); (3,3); (3,6); (6,0); (6,3); (6,6)]
  in
  rows_valid && cols_valid && subgrids_valid

(* Generate a random row of numbers from 1 to 9 *)
let randomize lst =
  let row = Array.of_list lst in 
  for i = 8 downto 1 do
    let j = Random.int (i + 1) in
    let temp = row.(i) in
    row.(i) <- row.(j);
    row.(j) <- temp;
  done;
  Array.to_list row
(**check if a given input is valid to be placed in the puzzle*)
let is_valid grid row col num =
  let box_row = (row / 3) * 3 in
  let box_col = (col / 3) * 3 in
  let rec check i = if i < 9 then
      if grid.(row).(i) = num || grid.(i).(col) =  num ||
         grid.(box_row + (i / 3)).(box_col + (i mod 3)) =  num
      then false else check (i + 1)
    else true
  in check 0

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




let generate () : t =
  let puzzle = Array.make_matrix 9 9 None in
  let solution = Array.make_matrix 9 9 0 in
  ignore (fill_grid solution 0 0);

  (* Create a puzzle by removing some numbers from the solution *)
  let remove_numbers count =
    let rec aux n =
      if n <= 0 then ()
      else
        let i = Random.int 9 in
        let j = Random.int 9 in
        if puzzle.(i).(j) = None then (
          puzzle.(i).(j) <- Some (solution.(i).(j));
          aux (n - 1)
        ) else aux n
    in
    aux count
  in
  
  remove_numbers (Random.int 40 + 20); (* Remove between 20 and 60 numbers *)
  
  { puzzle; solution }

  let get_puzzle (sudoku : t) : int option list list =
    Array.to_list (Array.map Array.to_list sudoku.puzzle)

    let get_solution (sudoku : t) : int list list =
    Array.to_list (Array.map Array.to_list sudoku.solution)


  let check (user_solution : int option list list) (gen_solution : t) =
    let user_array = Array.of_list (List.map Array.of_list user_solution) in
    let solution_array = gen_solution.solution in
    let is_valid = ref true in
    for i = 0 to 8 do
      for j = 0 to 8 do
        match user_array.(i).(j) with
        | Some num when num <> solution_array.(i).(j) -> is_valid := false
        | _ -> ()
      done
    done;
    !is_valid
    


(* Create a new empty sudoku puzzle *)