(** Times puzzle generation. Run with: dune exec bench/bench.exe -- [N]
    where N is the number of puzzles to generate (default 20). *)
let () =
  let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 20 in
  let times =
    List.init n (fun _ ->
        let start = Unix.gettimeofday () in
        ignore (Sudoku.Puzzles.generate ());
        Unix.gettimeofday () -. start)
  in
  let total = List.fold_left ( +. ) 0. times in
  let worst = List.fold_left max 0. times in
  Printf.printf "Generated %d puzzles: total %.3fs, average %.4fs, slowest %.4fs\n"
    n total (total /. float_of_int n) worst
