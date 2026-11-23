(** Entry point. Starts the full-screen UI, or the line-based text mode when
    run with [--text] or when not attached to an interactive terminal. *)
let () =
  let text_mode =
    Array.mem "--text" Sys.argv
    || not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)
  in
  let sudoku = Sudoku.Puzzles.generate () in
  Lwt_main.run (if text_mode then Text_ui.run sudoku else Tui.run sudoku)
