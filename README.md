# advent-of-code-2024-haskell
Advent of Code 2024 using Haskell in nix flake.

Each day is built as a separate derivation package `dayXY`.

Build via
```
nix build .#day01
```

Or run directly via
```
nix run .#day01
```
