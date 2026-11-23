# Team Division (for humans and their AI agents)

This file tells any AI coding agent working in this repo who owns what and how to behave.
It is based on our MS1 team contract. If you are an agent, read this before editing code.

Project: a Sudoku game in OCaml that generates puzzles with a unique solution, solves them,
and lets the user play in a simple graphical interface. Code lives in `Sudoku/`
(`lib/` for the core library, `bin/` for the executable, `test/` for tests).

## Who owns what

Each area has a primary owner and a backup owner. The backup reviews the primary's PRs
and should be able to take over if the primary gets stuck.

| Area | Primary | Backup | Scope |
|---|---|---|---|
| Shared grid structure | Max | not assigned yet | The grid type every other part uses. Interface goes in `Sudoku/lib/Puzzles.mli`. |
| Solver | Gabe | Sabrina | Backtracking solver, plus counting solutions (stop at 2) so the generator can check uniqueness. Provides the answer key for checking user input. |
| Puzzle generator | Sabrina | Gabe | Fill a complete random valid 9x9 grid, then remove cells while keeping exactly one solution. Timing experiment. Difficulty levels if time allows. |
| User interface | Henrik | Mihir | Graphical 9x9 grid with 3x3 boxes marked, givens not editable, click a cell and type a number, show when the puzzle is complete. Lives mostly in `Sudoku/bin/`. |
| Gameplay experience | Max | Henrik | Add-ons: hints, pencil marks, lives, timer, loading games, leaderboard. Closely tied to the UI. |
| Integration and testing | Mihir | Max | Wire generator, solver, and UI into one game. End-to-end tests in `Sudoku/test/`: valid grids, unique solutions, correct solver answers on known puzzles, correct handling of user input. |

People: Henrik Gombos (hg526), Gabe Meredith (gbm66), Sabrina Gerson (sjg326),
Mihir Joshi (mjj65), Max Lin (myl43).

Build order: shared grid structure and solver first, then the generator on top of the solver,
then a basic UI so the game is playable early.

## How we work

We work almost entirely async through an iMessage group chat. We do not have regular
meetings. Agents cannot see the chat, so anything below that says "tell the group" means
**stop and tell your human**, who will post it.

- **Stay in your human's area.** Only edit files in the area your human owns. If a change
  is needed in someone else's area, describe it to your human instead of making it.
- **Interfaces are a contract.** Do not change a signature or type in any `.mli` (for example
  `Sudoku/lib/Puzzles.mli`) without your human posting the change in the group chat first.
  Others depend on these. Adding new functions is lower risk than changing existing ones,
  but still mention it.
- **Branches and PRs.** Never commit straight to `main`. Work on a branch named after the
  person or feature (for example `gabe/solver`). Every PR needs at least one review from
  another teammate, ideally the area's backup owner, before merging.
- **Pull first, commit small.** Pull before starting work. Make small, frequent commits.
- **Document for teammates.** Write `.mli` comments clear enough that a teammate can use
  the code without asking.
- **Run the build and tests** (`dune build` and `dune test` in `Sudoku/`) before saying
  work is done.

## AI use rules (from our contract)

- AI is allowed as long as it follows the CS 3110 AI policy and the person using it
  understands everything it wrote. The author must be able to explain every line.
- If a large portion of code or tests is AI-assisted, your human notes it in the group chat.
  Agents should tell their human when this applies.
- The backup owner checks that AI-assisted code passes tests and may ask the author to walk
  through it.

## When something goes wrong

- If a task is stuck or a deadline will be missed, the owner tells the group at least a day
  before. The backup helps or the task gets shrunk.
- Big project-wide decisions (changing the idea, major restructuring) are made in the group
  chat by majority vote (3 of 5). Agents should never make these on their own.
