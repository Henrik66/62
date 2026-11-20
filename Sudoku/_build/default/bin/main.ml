
(**Generates and prints a sudoku puzzle in the terminal*)
let () = 
  let sudoku = Sudoku.Puzzles.generate () in
  let puzzle = Sudoku.Puzzles.get_solution sudoku in
  List.iter (fun row ->
    List.iter (fun cell ->
      Printf.printf "%d " cell
    ) row;
    Printf.printf "\n") puzzle