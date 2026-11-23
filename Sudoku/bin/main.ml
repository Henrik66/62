open Lwt.Syntax

let render_grid grid =
  let print_separator () = Printf.printf "+---------+---------+---------+\n" in
  print_separator ();
  List.iteri
    (fun row_index row ->
      Printf.printf "|";
      List.iteri
        (fun col_index cell ->
          (match cell with
          | None -> Printf.printf " . "
          | Some n -> Printf.printf " %d " n);
          if col_index mod 3 = 2 then Printf.printf "|")
        row;
      Printf.printf "\n";
      if row_index mod 3 = 2 then print_separator ())
    grid;
  flush stdout

let see_solution sudoku =
  let solution =
    Sudoku.Puzzles.get_solution sudoku
    |> List.map (List.map (fun n -> Some n))
  in
  render_grid solution

let render sudoku = Sudoku.Puzzles.get_puzzle sudoku |> render_grid

(* Returns the updated sudoku board (or the original one if the move was invalid) *)
let apply_move sudoku input =
  match String.split_on_char ' ' (String.trim input) with
  | [ r; c; v ] -> (
      try
        let row = int_of_string r in
        let col = int_of_string c in
        let value = int_of_string v in
        if Sudoku.Puzzles.validate value row col sudoku then
          (Printf.printf "you went row %d, col %d, val %d\n" row col value;
          Sudoku.Puzzles.set value row col sudoku)
        else (
          Printf.printf "Invalid move. Try again.\n";
          sudoku)
      with Failure _ ->
        Printf.printf "Please enter valid numbers.\n";
        sudoku)
  | _ ->
      Printf.printf "Invalid input. Use: row col val\n";
      sudoku

let rec game_loop sudoku =
  render sudoku;
  let* () = Lwt_io.print "\nEnter move (row col val) or 'quit' or 'solution': " in
  let* line_opt = Lwt_io.read_line_opt Lwt_io.stdin in
  match line_opt with

  | None | Some "quit" ->
      Lwt_io.printl "Goodbye!"
  | Some "solution" ->
      see_solution sudoku;
      game_loop sudoku
  | Some input ->
      let updated_sudoku = apply_move sudoku input in
      game_loop updated_sudoku
let () =
  let sudoku = Sudoku.Puzzles.generate () in
  Lwt_main.run (game_loop sudoku)
