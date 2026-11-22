
(**Generates and prints a sudoku puzzle in the terminal*)
let () = 
  let sudoku = Sudoku.Puzzles.generate () in
  let puzzle = Sudoku.Puzzles.get_solution sudoku in
  (List.iter (fun row ->
    List.iter (fun cell ->
      Printf.printf "%d " cell
    ) row;
    Printf.printf "\n") puzzle);
    Printf.printf "Puzzle is valid: %b\n" (Sudoku.Puzzles.rep_ok sudoku);

    (List.iter (fun row ->
    List.iter (fun cell ->
      match cell with
      | Some n -> Printf.printf "%d " n
      | None -> Printf.printf ". "
    ) row;
    Printf.printf "\n") (Sudoku.Puzzles.get_puzzle sudoku))

    
