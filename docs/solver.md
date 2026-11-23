# Solver interface

Owner: Gabe (backup: Sabrina). Code: `Sudoku/lib/solver.mli`, `Sudoku/lib/solver.ml`.

## Types

```ocaml
type puzzle = int option array array
```

A 9x9 grid of rows. `Some n` is a filled cell (1 to 9), `None` is empty.
This matches the grid `Puzzles` uses internally, so the generator can pass its
puzzle straight in.

## Functions

| Function | Returns |
|---|---|
| `Solver.solve : puzzle -> int array array option` | `Some grid` with every empty cell filled and every clue kept, or `None` if there is no answer. |
| `Solver.count_solutions : puzzle -> int` | `0`, `1`, or `2` (2 means "two or more"; it stops searching early). |

Neither function changes the grid you pass in.

Both return "no solution" (`None` / `0`) right away if the clues already break
a rule (for example two 5s in one row).

## How it works (short version)

Backtracking: pick an empty cell, try each number that fits, recurse, undo if
it leads to a dead end.

The speed trick is picking the cell with the **fewest** numbers that fit, not
just the next empty one. A cell with one option is forced, and a cell with zero
options means "back up now", so the search wastes much less time.

## Who uses it

- `Puzzles.generate` calls `Solver.count_solutions` after removing each clue,
  and keeps the removal only if the answer is still 1.
- Tests (and later the UI) can call `Solver.solve` on any puzzle, for example
  puzzles typed in from outside.

## Timing (generator, 50 puzzles, `dune exec Sudoku/bench/bench.exe -- 50`)

| Version | Average | Slowest |
|---|---|---|
| Old solver (next empty cell) | 0.44s | 3.63s |
| New solver (fewest options first) | 0.11s | 1.07s |

Answer to the MS1 question "is generating unique puzzles fast enough?": yes.
