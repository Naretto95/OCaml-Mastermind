# OCaml Mastermind

A terminal implementation of **Mastermind**, the classic code-breaking game, written in OCaml. Two can play: you against the computer, or the computer against you — guessing each other's secret 4-peg code in alternating rounds, over as many rounds as you like.

Originally built as a school project; rewritten as a small, idiomatic OCaml codebase built with [Dune](https://dune.build/).

## Features

- **Two game directions** — guess the AI's code, or have the AI guess yours, alternating each round.
- **Two AI difficulties**:
  - *Aleatoire* — guesses uniformly at random.
  - *Knuth (minimax)* — implements [Knuth's five-guess algorithm](https://en.wikipedia.org/wiki/Mastermind_(board_game)#Five-guess_algorithm), always picking the guess that minimizes the worst-case number of remaining candidates.
- **Configurable matches** — pick your name, the number of attempts per round, and the number of rounds.
- **Optional auto-answer** — let the program score the AI's guesses against your code automatically, or enter black/white peg counts yourself.
- Colorized terminal output.

## Project layout

```
lib/            Core game logic, no I/O
  peg.ml          The 6 peg colors: parsing, display, ANSI coloring
  secret_code.ml  Codes: generation, string conversion, feedback scoring
  solver.ml       AI strategies (random guessing, Knuth minimax)
  ansi.ml         ANSI escape code helpers
  ui.ml           Small cross-platform terminal helpers (clear screen, pause)
bin/
  main.ml         The interactive CLI that drives a match
test/
  test_secret_code.ml   Unit tests for the pure game logic
docs/
  Rapport.pdf     Original project report (French)
```

The `lib` modules are pure and unit-tested; `bin/main.ml` is the terminal front-end that reads input, prints the board, and drives the game loop.

## Requirements

- OCaml (>= 4.08) and [Dune](https://dune.build/) (>= 3.0)

No third-party libraries are required.

## Building and running

```bash
dune build
dune exec bin/main.exe
```

## Running the tests

```bash
dune test
```

## How it plays

The game is entirely in French (the original language of the project): pseudo (player name), number of attempts, number of rounds, and whether you want the program to auto-answer the AI's guesses. Codes are 4 pegs chosen from `rouge`, `bleu`, `vert`, `jaune`, `orange`, `violet`, entered space-separated, e.g. `rouge bleu vert jaune`.

Each round, one side is the code-setter and the other guesses:
- **You guess**: enter a code each turn, get told how many pegs are the right color in the right place (*noirs*) and right color in the wrong place (*blancs*), until you find it or run out of attempts.
- **The AI guesses**: it proposes codes using the selected strategy; you tell it the score (or let auto-answer do it), until it breaks your code or runs out of attempts.

Whoever finds the other's code wins the round; after all rounds are played, the higher score wins the match.

## License

MIT — see [LICENSE](LICENSE).
