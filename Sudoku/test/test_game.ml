open OUnit2
open Sudoku

(* One generated puzzle shared by every test. [Game] never modifies it. *)
let sudoku = Puzzles.generate ()
let puzzle = Puzzles.get_puzzle sudoku |> List.map Array.of_list |> Array.of_list
let solution = Puzzles.get_solution sudoku |> List.map Array.of_list |> Array.of_list

(** [first p] is the first (row, col) in reading order where [p row col]. *)
let first p =
  let rec go i =
    if i = 81 then failwith "no such cell"
    else if p (i / 9) (i mod 9) then (i / 9, i mod 9)
    else go (i + 1)
  in
  go 0

let empty_cells =
  List.filter (fun (r, c) -> puzzle.(r).(c) = None)
    (List.init 81 (fun i -> (i / 9, i mod 9)))

let given_rc = first (fun r c -> puzzle.(r).(c) <> None)
let empty_rc = first (fun r c -> puzzle.(r).(c) = None)
let wrong_value (r, c) = (solution.(r).(c) mod 9) + 1

let at (r, c) = Game.select (Game.create sudoku) r c

let tests =
  [
    ( "new game shows the puzzle" >:: fun _ ->
      let g = Game.create sudoku in
      for r = 0 to 8 do
        for c = 0 to 8 do
          let expected =
            match puzzle.(r).(c) with Some n -> Game.Given n | None -> Game.Empty
          in
          assert_equal expected (Game.cell g r c)
        done
      done );
    ( "cursor starts top-left" >:: fun _ ->
      assert_equal (0, 0) (Game.cursor (Game.create sudoku)) );
    ( "move stops at edges" >:: fun _ ->
      let g = Game.create sudoku in
      assert_equal (0, 0) (Game.cursor (Game.move g (-1) (-1)));
      assert_equal (8, 8) (Game.cursor (Game.move g 20 20));
      assert_equal (1, 2) (Game.cursor (Game.move g 1 2)) );
    ( "select clamps" >:: fun _ ->
      let g = Game.create sudoku in
      assert_equal (3, 4) (Game.cursor (Game.select g 3 4));
      assert_equal (0, 8) (Game.cursor (Game.select g (-5) 99)) );
    ( "givens cannot be changed" >:: fun _ ->
      let r, c = given_rc in
      let g = at given_rc in
      let before = Game.cell g r c in
      assert_equal before (Game.cell (Game.enter g (wrong_value given_rc)) r c);
      assert_equal before (Game.cell (Game.clear g) r c) );
    ( "correct entry" >:: fun _ ->
      let r, c = empty_rc in
      let g = Game.enter (at empty_rc) solution.(r).(c) in
      assert_equal (Game.Correct solution.(r).(c)) (Game.cell g r c);
      assert_equal 0 (Game.wrong_count g) );
    ( "wrong entry" >:: fun _ ->
      let r, c = empty_rc in
      let v = wrong_value empty_rc in
      let g = Game.enter (at empty_rc) v in
      assert_equal (Game.Wrong v) (Game.cell g r c);
      assert_equal 1 (Game.wrong_count g) );
    ( "clear removes entry" >:: fun _ ->
      let r, c = empty_rc in
      let g = Game.clear (Game.enter (at empty_rc) 5) in
      assert_equal Game.Empty (Game.cell g r c) );
    ( "out of range digits ignored" >:: fun _ ->
      let r, c = empty_rc in
      let g = at empty_rc in
      assert_equal Game.Empty (Game.cell (Game.enter g 0) r c);
      assert_equal Game.Empty (Game.cell (Game.enter g 10) r c) );
    ( "old states are unchanged" >:: fun _ ->
      let r, c = empty_rc in
      let g = at empty_rc in
      let _ = Game.enter g 5 in
      assert_equal Game.Empty (Game.cell g r c) );
    ( "complete only when all cells correct" >:: fun _ ->
      let fill g (r, c) v = Game.enter (Game.select g r c) v in
      let g = Game.create sudoku in
      assert_bool "new game not complete" (not (Game.is_complete g));
      let solved =
        List.fold_left (fun g (r, c) -> fill g (r, c) solution.(r).(c)) g empty_cells
      in
      assert_bool "solved" (Game.is_complete solved);
      let broken = fill solved empty_rc (wrong_value empty_rc) in
      assert_bool "one wrong cell" (not (Game.is_complete broken)) );
  ]

let () = run_test_tt_main ("Game" >::: tests)
