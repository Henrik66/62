(** Interactive full-screen terminal UI built on lambda-term. The player
    selects a cell with the mouse or the arrow keys and types a digit. All
    game rules live in [Sudoku.Game]; this module only draws and reads
    input. *)

open Lwt.Syntax
module Game = Sudoku.Game

(* Screen layout. The grid's top-left corner is at ([top], [left]). Each
   cell is 3 characters wide (" 5 ") and one line tall, with a border line
   or column between each 3x3 box. *)
let top = 2
let left = 2
let grid_width = 31
let grid_height = 13

(** [cell_y r] is the screen row of grid row [r]. *)
let cell_y r = top + 1 + r + (r / 3)

(** [cell_x c] is the screen column where grid column [c] starts. *)
let cell_x c = left + 1 + (3 * c) + (c / 3)

(** [find p] is [Some i] for the first [i] in 0..8 with [p i], else [None]. *)
let find p = List.find_opt p (List.init 9 Fun.id)

(** [cell_at y x] is [Some (row, col)] if screen position ([y], [x]) is
    inside a cell, or [None] if it is on a border or outside the grid. *)
let cell_at y x =
  match
    (find (fun r -> cell_y r = y), find (fun c -> x >= cell_x c && x < cell_x c + 3))
  with
  | Some r, Some c -> Some (r, c)
  | _ -> None

let zs = Zed_string.of_utf8
let plain = LTerm_style.none
let given_style = { plain with bold = Some true }
let correct_style = { plain with foreground = Some LTerm_style.lblue }
let wrong_style = { plain with foreground = Some LTerm_style.lred; bold = Some true }
let dim_style = { plain with foreground = Some LTerm_style.lblack }
let win_style = { plain with foreground = Some LTerm_style.lgreen; bold = Some true }

let draw_borders ctx =
  let line = "+---------+---------+---------+" in
  for k = 0 to 3 do
    LTerm_draw.draw_string ctx (top + (4 * k)) left (zs line)
  done;
  for r = 0 to 8 do
    for k = 0 to 3 do
      LTerm_draw.draw_string ctx (cell_y r) (left + (10 * k)) (zs "|")
    done
  done

let draw_cell ctx game r c =
  let text, style =
    match Game.cell game r c with
    | Game.Given n -> (Printf.sprintf " %d " n, given_style)
    | Game.Correct n -> (Printf.sprintf " %d " n, correct_style)
    | Game.Wrong n -> (Printf.sprintf " %d " n, wrong_style)
    | Game.Empty -> (" . ", dim_style)
  in
  let style =
    if Game.cursor game = (r, c) then { style with reverse = Some true }
    else style
  in
  LTerm_draw.draw_string ctx (cell_y r) (cell_x c) ~style (zs text)

let draw_status ctx game =
  let y = top + grid_height + 1 in
  let help = "Click or arrows: select   1-9: enter   0/Backspace: clear   q: quit" in
  LTerm_draw.draw_string ctx y left ~style:dim_style (zs help);
  if Game.is_complete game then
    LTerm_draw.draw_string ctx (y + 2) left ~style:win_style
      (zs "Solved! Press q to quit.")
  else
    match Game.wrong_count game with
    | 0 -> ()
    | n ->
        let msg =
          Printf.sprintf "%d wrong %s (shown in red)" n
            (if n = 1 then "entry" else "entries")
        in
        LTerm_draw.draw_string ctx (y + 2) left ~style:wrong_style (zs msg)

let draw game ui matrix =
  let ctx = LTerm_draw.context matrix (LTerm_ui.size ui) in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_string ctx 0 left ~style:given_style (zs "Sudoku");
  draw_borders ctx;
  for r = 0 to 8 do
    for c = 0 to 8 do
      draw_cell ctx game r c
    done
  done;
  draw_status ctx game

(** [handle game ev] is [Some g] where [g] is the game after event [ev], or
    [None] if [ev] means the player wants to quit. *)
let handle game ev =
  match ev with
  | LTerm_event.Key k -> (
      match LTerm_key.code k with
      | LTerm_key.Escape -> None
      | LTerm_key.Up -> Some (Game.move game (-1) 0)
      | LTerm_key.Down -> Some (Game.move game 1 0)
      | LTerm_key.Left -> Some (Game.move game 0 (-1))
      | LTerm_key.Right -> Some (Game.move game 0 1)
      | LTerm_key.Backspace | LTerm_key.Delete -> Some (Game.clear game)
      | LTerm_key.Char u -> (
          match Uchar.to_int u with
          | 0x71 | 0x51 -> None (* q or Q *)
          | 0x30 -> Some (Game.clear game) (* 0 *)
          | d when d >= 0x31 && d <= 0x39 -> Some (Game.enter game (d - 0x30))
          | _ -> Some game)
      | _ -> Some game)
  | LTerm_event.Mouse m when LTerm_mouse.button m = LTerm_mouse.Button1 -> (
      match cell_at (LTerm_mouse.row m) (LTerm_mouse.col m) with
      | Some (r, c) -> Some (Game.select game r c)
      | None -> Some game)
  | _ -> Some game

let rec loop ui state =
  let* ev = LTerm_ui.wait ui in
  match handle !state ev with
  | None -> Lwt.return_unit
  | Some g ->
      state := g;
      LTerm_ui.draw ui;
      loop ui state

(** [run sudoku] plays [sudoku] full-screen until the player quits. Requires:
    standard input and output are a terminal. *)
let run sudoku =
  let* term = Lazy.force LTerm.stdout in
  let state = ref (Game.create sudoku) in
  let* ui = LTerm_ui.create term (fun ui matrix -> draw !state ui matrix) in
  let* () = LTerm.enable_mouse term in
  Lwt.finalize
    (fun () -> loop ui state)
    (fun () ->
      let* () = LTerm.disable_mouse term in
      LTerm_ui.quit ui)
