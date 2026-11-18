
(**Generates and prints a sudoku puzzle in the terminal*)
let () = 
  let sudoku = Sudoku.Puzzles.generate () in
  let puzzle = Sudoku.Puzzles.get_puzzle sudoku in
  List.iter (fun row ->
    List.iter (fun cell ->
      match cell with
      | Some num -> Printf.printf "%d " num
      | None -> Printf.printf ". "
    ) row;
    Printf.printf "\n") puzzle