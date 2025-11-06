# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a competitive programming repository for solving Advent of Code puzzles (https://everybody.codes/events), primarily implemented in Haskell. The codebase contains solutions organized by year (2021-2025) with a modular day-by-day structure.

## Development Environment

This project uses Nix flakes for reproducible development environments:

```bash
# Enter development shell
nix develop

# Build the project
cabal build

# Run the current solution (configured in Main.hs)
cabal run aoc-executable
```

## Project Structure

```
src/
  Main.hs              # Entry point - imports and runs specific day's solution
  Y2021/Day*.hs        # 2021 solutions
  Y2022/Day*.hs        # 2022 solutions
  Y2023/Day*.hs        # 2023 solutions
  Y2024/Day*.hs        # 2024 solutions (complete Days 1-25)
data/
  YYYY/day*.txt        # Input files organized by year
```

Each day module exports `partI` and `partII` functions that solve the respective parts of that day's puzzle.

## Working on Solutions

### Running a Specific Day

Edit `src/Main.hs` to import and call the desired day's solution:

```haskell
module Main where

import Y2024.Day15 (partII)

main :: IO ()
main = partII
```

Then run with `cabal run aoc-executable`.

### Creating a New Solution

1. Create `src/YYYY/DayN.hs` following the standard module structure:
   ```haskell
   module YYYY.DayN (partI, partII) where

   partI :: IO ()
   partI = do
       input <- readFile "data/YYYY/dayN.txt"
       -- solution logic
       print result

   partII :: IO ()
   partII = do
       input <- readFile "data/YYYY/dayN.txt"
       -- solution logic
       print result
   ```

2. Add the module to `aoc.cabal` under `other-modules:`

3. Ensure the corresponding input file exists at `data/YYYY/dayN.txt`

## Code Style and Formatting

The project uses fourmolu for Haskell formatting with specific settings:

```bash
# Format code
nix fmt

# Or via treefmt directly
treefmt
```

Pre-commit hooks are configured to run formatting automatically.

## Language Extensions and Dependencies

The project enables many GHC extensions by default (see `aoc.cabal` common stanza), including:
- `ImportQualifiedPost` - use `import Data.Map qualified as M`
- `LambdaCase`, `ViewPatterns` - pattern matching conveniences
- `OverloadedStrings` - string literals work as Text/ByteString
- Many others for modern Haskell development

Common dependencies used across solutions:
- `attoparsec` / `megaparsec` - parsing input
- `containers` - Map, Set, IntMap, etc.
- `vector` - efficient arrays
- `split` - string splitting utilities
- `lens` - optics for data manipulation
- `linear` - vector/matrix operations
- `fgl` - functional graph library

## Architecture Notes

- Solutions are self-contained modules that read their own input files
- Common pattern: parse input → transform → compute → print result
- Input files follow the convention `data/YYYY/dayN.txt`
- The main executable is configured at build time via `Main.hs`
- Tests are currently commented out in the cabal file
- The project includes some alternative implementations in `elisp/` and `zig/` directories (not actively maintained)
