(*Representation of sudoku puzzles and generators*)
type t = { puzzle : int option array array; solution : int array array }

let generate () : t =
  let puzzle = Array.make_matrix 9 9 None in
  let solution = Array.make_matrix 9 9 0 in
  (* Fill the solution with a valid sudoku solution *)
  (* This is a placeholder for the actual sudoku generation logic *)
  for i = 0 to 8 do
    for j = 0 to 8 do
      solution.(i).(j) <- (i * 3 + i / 3 + j) mod 9 + 1
    done
  done;
  
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