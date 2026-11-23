open OUnit2
open Sudoku

(* Tests for [Solver] using known puzzles with published solutions. These add
   to the tests in test_solver.ml rather than repeating them. Every answer
   key below was checked with a separate solver outside this project, so the
   solver is not grading itself. *)

(* ---------- Helpers ---------- *)

(** [parse s] turns an 81-character string (read row by row, ['.'] or ['0']
    for empty) into a puzzle. Whitespace is ignored so puzzles can be written
    one row per group. *)
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

(** [grid_of_string s] is the completed grid written in [s]. Requires: [s]
    has no empty cells. *)
let grid_of_string (s : string) : int array array =
  Array.map (Array.map (function Some n -> n | None -> 0)) (parse s)

(** [clear cells g] is the puzzle made from completed grid [g] with every
    ([row], [col]) in [cells] emptied. *)
let clear (cells : (int * int) list) (g : int array array) : Solver.puzzle =
  let p = Array.map (Array.map (fun n -> Some n)) g in
  List.iter (fun (r, c) -> p.(r).(c) <- None) cells;
  p

(** [string_of_answer a] shows a result of [Solver.solve] one row per line,
    so a failing test prints a readable grid instead of [<abstr>]. *)
let string_of_answer (a : int array array option) : string =
  match a with
  | None -> "None"
  | Some g ->
      g
      |> Array.map (fun row ->
             String.concat "" (Array.to_list (Array.map string_of_int row)))
      |> Array.to_list |> String.concat "\n" |> ( ^ ) "\n"

(* ---------- Known puzzles with published solutions ---------- *)

(* Project Euler problem 96, grid 01. *)
let euler =
  "..3.2.6.. 9..3.5..1 ..18.64.. ..81.29.. 7.......8 ..67.82.. ..26.95.. \
   8..2.3..9 ..5.1.3.."

let euler_solution =
  "483921657 967345821 251876493 548132976 729564138 136798245 372689514 \
   814253769 695417382"

(* Arto Inkala's 2012 puzzle, widely published as one of the hardest for
   humans. *)
let inkala =
  "8........ ..36..... .7..9.2.. .5...7... ....457.. ...1...3. ..1....68 \
   ..85...1. .9....4.."

let inkala_solution =
  "812753649 943682175 675491283 154237896 369845721 287169534 521974368 \
   438526917 796318452"

(* Wikipedia's example solution, used as a starting point for building other
   puzzles. *)
let wiki_solution =
  "534678912 672195348 198342567 859761423 426853791 713924856 961537284 \
   287419635 345286179"

(* ---------- Puzzles with no solution ---------- *)

(* test_solver.ml covers two 5s in a row. These check the column and box
   rules, which a row-only bug would miss. *)
let dup_in_column = "5........ 5........" ^ String.make 63 '.'

(* The two 5s share a box but not a row or a column, so only the box rule
   can catch them. *)
let dup_in_box = "5........ .5......." ^ String.make 63 '.'

(* ---------- Test builders ---------- *)

(** [solves_known name puzzle solution] checks that [solve] returns exactly
    the published [solution]. *)
let solves_known name puzzle solution =
  name ^ " matches the published solution" >:: fun _ ->
  assert_equal ~printer:string_of_answer
    (Some (grid_of_string solution))
    (Solver.solve (parse puzzle))

(** [unsolvable name puzzle] checks that [solve] is [None] and
    [count_solutions] is [0]. *)
let unsolvable name puzzle =
  name ^ ": no solution" >:: fun _ ->
  assert_equal ~printer:string_of_answer None (Solver.solve (parse puzzle));
  assert_equal ~printer:string_of_int 0 (Solver.count_solutions (parse puzzle))

(* ---------- Suite: lists the tests and runs them ---------- *)

let suite =
  let wiki = grid_of_string wiki_solution in
  "Solver test suite"
  >::: [
         solves_known "Project Euler 96 grid 01" euler euler_solution;
         solves_known "Inkala hardest" inkala inkala_solution;
         ( "any one empty cell is filled back in" >:: fun _ ->
           (* Try all 81 cells, one at a time *)
           for r = 0 to 8 do
             for c = 0 to 8 do
               let msg = Printf.sprintf "cell (%d, %d)" r c in
               assert_equal ~msg ~printer:string_of_answer (Some wiki)
                 (Solver.solve (clear [ (r, c) ] wiki))
             done
           done );
         ( "puzzle with exactly two answers counts as 2" >:: fun _ ->
           (* These four cells hold 1 3 / 3 1, and 3 1 / 1 3 also works, so
              emptying them leaves two answers. The generator relies on this
              count to reject puzzles that are not unique. *)
           let p = clear [ (3, 5); (3, 8); (4, 5); (4, 8) ] wiki in
           assert_equal ~printer:string_of_int 2 (Solver.count_solutions p) );
         unsolvable "repeat in a column" dup_in_column;
         unsolvable "repeat in a box only" dup_in_box;
       ]

(* Runs the suite and prints results. This is what dune test executes. *)
let () = run_test_tt_main suite
