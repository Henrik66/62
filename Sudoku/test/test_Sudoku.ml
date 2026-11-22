open OUnit2
open Sudoku.Puzzles

(* A group is a row, a column, or a 3x3 box (a list of 9 numbers).
   Sorting it must give exactly 1 to 9, which means no repeats and
   nothing missing. *)
let valid_group l = List.sort compare l = [1;2;3;4;5;6;7;8;9]

(* Checks a whole solution (a list of 9 rows, each a list of 9 ints) *)
let solution_is_valid sol =
  let col c = List.map (fun r -> List.nth r c) sol in

  (* Box number b goes 0 to 8, left to right, top to bottom.
     b / 3 gives the box row and b mod 3 gives the box column.
     r0 and c0 are the top-left cell of that box. *)
  let box b =
    let r0 = (b / 3) * 3 and c0 = (b mod 3) * 3 in
    (* For each of the 3 rows in the box, keep only the 3 cells that
       fall in the box columns, then join the results into one list of 9 *)
    List.concat_map
      (fun r -> List.filteri (fun c _ -> c >= c0 && c < c0 + 3) (List.nth sol r))
      [ r0; r0 + 1; r0 + 2 ]
  in

  (* Indices 0 to 8, used to loop over columns and boxes *)
  let idx = [0;1;2;3;4;5;6;7;8] in

  (* All 27 groups must be valid: 9 rows, 9 columns, 9 boxes *)
  List.for_all valid_group sol
  && List.for_all (fun c -> valid_group (col c)) idx
  && List.for_all (fun b -> valid_group (box b)) idx

(* Turns the stored solution into the shape check expects: every digit
   wrapped in Some. This is what a user who filled in the whole board
   correctly would submit. *)
let full_of s = List.map (List.map (fun n -> Some n)) (get_solution s)

(* ---------- Test 1: generated solutions follow the Sudoku rules ---------- *)

let test_solutions_valid _ =
  (* Repeat 50 times because the generator is random, and one pass
     could pass by luck *)
  for _ = 1 to 50 do
    assert_bool "invalid solution" (solution_is_valid (get_solution (generate ())))
  done

(* ---------- Test 2: every given agrees with the solution ---------- *)

let test_puzzle_matches_solution _ =
  for _ = 1 to 50 do
    let s = generate () in
    List.iter2
      (fun prow srow ->
        List.iter2
          (fun p v ->
            match p with
            | None -> ()
            | Some n -> assert_equal v n)
          prow srow)
      (get_puzzle s) (get_solution s)
  done

(* ---------- Test 3: check accepts the correct full solution ---------- *)

let test_check_accepts_solution _ =
  let s = generate () in
  (* Submitting the true solution must be judged correct *)
  assert_bool "true solution rejected" (check (full_of s) s)

(* ---------- Test 4: check rejects a board with one wrong digit ---------- *)

let test_check_rejects_wrong _ =
  let s = generate () in
  let wrong =
    (* Start from the correct full board and change only cell row 0, column 0 *)
    List.mapi
      (fun i row ->
        List.mapi
          (fun j c ->
            match c with
            | Some n when i = 0 && j = 0 -> Some ((n mod 9) + 1)
            | _ -> c)
          row)
      (full_of s)
  in
  (* check must return false *)
  assert_bool "wrong entry accepted" (not (check wrong s))

(* ---------- Suite: lists the tests and runs them ---------- *)

let suite =
  (* The string is the suite name. Each entry is a test name >:: a function. *)
  "puzzle tests"
  >::: [
         "solutions valid" >:: test_solutions_valid;
         "puzzle matches solution" >:: test_puzzle_matches_solution;
         "check accepts true solution" >:: test_check_accepts_solution;
         "check rejects wrong entry" >:: test_check_rejects_wrong;
       ]

(* Runs the suite and prints results. This is what dune test executes. *)
let () = run_test_tt_main suite