# while

This is an experiment with simple static analyses for a while language compiling to control flow graphs.

## `src/ConstraintSystem.hs`

Contains a generic solver for constraints where the lattice is on sets.

## `src/CFG.hs`

Contains the syntax definitions, and a very simple compiler from a while language to a CFG. The CFG has commands on edges.

## `src/Analysis.hs`

Contains a few very simple analyses.

- Available expressions
- True liveness

## `src/Parser.hs`

Simple parser for the while language.
