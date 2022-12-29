# Totally Entirely Accurate Bible Quote Generator

A strange and quite stupid little tool to generate inaccurate bible quotes that
are kind of sort of real quotes from the bible. Kind of hard to explain so for example:

> "god...is...the...fish" (Ge1:1, Ge1:11, Ge1:1, Ge1:26).

###### And yes I know this isn't really how ellipses work

## Building and Usage

The project is based around haskell (GHC) and cabal, and is known to build with ghc 9.2.3

```bash
❯ cabal run totally-accurate-bible-quotes -- god is the real fish
Up to date
Building word lookup tree...
"god...is...the...fish" (Ge1:1, Ge1:11, Ge1:1, Ge1:26).
```
