open OUnit2
open Sudoku

(** [parse s] turns an 81-character string (read row by row, ['.'] or ['0']
    for empty) into a puzzle. Whitespace is ignored so puzzles can be written
    one row per line. *)
let parse (s : string) : Solver.puzzle =
  let chars =
    String.to_seq s
    |> Seq.filter (fun c -> c <> ' ' && c <> '\n')
    |> Array.of_seq
  in
  assert (Array.length chars = 81);
  Array.init 9 (fun r ->
      Array.init 9 (fun c ->
          match chars.((r * 9) + c) with
          | '.' | '0' -> None
          | d -> Some (Char.code d - Char.code '0')))

(** [is_complete_valid g] is [true] if every row, column, and box of [g]
    contains exactly the numbers 1 through 9. *)
let is_complete_valid (g : int array array) : bool =
  let group_ok cells =
    List.sort compare cells = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
  in
  let idx = List.init 9 Fun.id in
  List.for_all
    (fun i ->
      group_ok (List.map (fun j -> g.(i).(j)) idx)
      && group_ok (List.map (fun j -> g.(j).(i)) idx)
      && group_ok
           (List.map (fun j -> g.((i / 3 * 3) + (j / 3)).((i mod 3 * 3) + (j mod 3))) idx))
    idx

(** [keeps_givens p g] is [true] if every filled cell of [p] has the same
    value in [g]. *)
let keeps_givens (p : Solver.puzzle) (g : int array array) : bool =
  let ok = ref true in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c cell ->
          match cell with Some n when g.(r).(c) <> n -> ok := false | _ -> ())
        row)
    p;
  !ok

(* Wikipedia's example puzzle, with its published solution. *)
let easy =
  "53..7.... 6..195... .98....6. 8...6...3 4..8.3..1 7...2...6 .6....28. \
   ...419..5 ....8..79"

let easy_solution =
  "534678912 672195348 198342567 859761423 426853791 713924856 961537284 \
   287419635 345286179"

(* A puzzle widely published as one of the hardest for humans. *)
let hard =
  "8........ ..36..... .7..9.2.. .5...7... ....457.. ...1...3. ..1....68 \
   ..85...1. .9....4.."

let empty = String.make 81 '.'

(* Two 5s in the first row: no solution can exist. *)
let conflicting_givens = "55" ^ String.make 79 '.'

(* Every given is legal on its own (no repeats), but the top-right cell
   needs a 9 to finish row 0, and column 8 already has a 9 below it. *)
let unsolvable = "12345678. ........9" ^ String.make 63 '.'

(* Only one row is partly filled, so there are many ways to finish. *)
let multi = "12345678." ^ String.make 72 '.'

let grid_of_string s =
  let p = parse s in
  Array.map (Array.map (function Some n -> n | None -> 0)) p

let solves_to name puzzle =
  name >:: fun _ ->
  let p = parse puzzle in
  match Solver.solve p with
  | None -> assert_failure "expected a solution, got None"
  | Some g ->
      assert_bool "solution breaks a rule" (is_complete_valid g);
      assert_bool "solution changed a given" (keeps_givens p g)

let tests =
  "Solver"
  >::: [
         ( "easy puzzle matches the published solution" >:: fun _ ->
           assert_equal (Some (grid_of_string easy_solution))
             (Solver.solve (parse easy)) );
         solves_to "hard puzzle is solved validly" hard;
         solves_to "empty grid is solved validly" empty;
         ( "easy puzzle has exactly one solution" >:: fun _ ->
           assert_equal ~printer:string_of_int 1 (Solver.count_solutions (parse easy)) );
         ( "hard puzzle has exactly one solution" >:: fun _ ->
           assert_equal ~printer:string_of_int 1 (Solver.count_solutions (parse hard)) );
         ( "empty grid counts as 2 (capped)" >:: fun _ ->
           assert_equal ~printer:string_of_int 2 (Solver.count_solutions (parse empty)) );
         ( "nearly empty grid counts as 2" >:: fun _ ->
           assert_equal ~printer:string_of_int 2 (Solver.count_solutions (parse multi)) );
         ( "conflicting givens: solve is None" >:: fun _ ->
           assert_equal None (Solver.solve (parse conflicting_givens)) );
         ( "conflicting givens: count is 0" >:: fun _ ->
           assert_equal ~printer:string_of_int 0
             (Solver.count_solutions (parse conflicting_givens)) );
         ( "legal but unsolvable: solve is None" >:: fun _ ->
           assert_equal None (Solver.solve (parse unsolvable)) );
         ( "legal but unsolvable: count is 0" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 (Solver.count_solutions (parse unsolvable)) );
         ( "solved grid solves to itself" >:: fun _ ->
           assert_equal (Some (grid_of_string easy_solution))
             (Solver.solve (parse easy_solution)) );
         ( "solve does not modify its input" >:: fun _ ->
           let p = parse easy in
           let before = Array.map Array.copy p in
           ignore (Solver.solve p);
           ignore (Solver.count_solutions p);
           assert_equal before p );
         ( "generated puzzles: unique, and solver matches the answer key" >:: fun _ ->
           for _ = 1 to 5 do
             let s = Puzzles.generate () in
             let p = Array.of_list (List.map Array.of_list (Puzzles.get_puzzle s)) in
             let key = Array.of_list (List.map Array.of_list (Puzzles.get_solution s)) in
             assert_equal ~printer:string_of_int 1 (Solver.count_solutions p);
             assert_equal (Some key) (Solver.solve p)
           done );
       ]

let () = run_test_tt_main tests
