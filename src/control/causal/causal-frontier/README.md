# causal-frontier

## causal-frontier

The causal partial order whose totally-ordered quotient is rosette-game-protocol. A global tick is a consensus artifact (TiDi -- pause the world so everyone agrees who was first); the honest object is a causal partial order: events carry hybrid logical clocks (causality + a near-physical-time bound in bounded size) and explicit causal dependencies (a reactive frontier), genuinely-concurrent events have no world-fact about their order and are settled only by a deterministic HLC-then-id tiebreak, and each state cell is adjudicated by exactly one arbiter so contention stays local. Two laws are gate-proved: QUOTIENT -- adjudicating the causal order equals rosette-game-protocol:run-game over any deterministic linearization; PARTITION-INVARIANCE -- the per-cell final state is independent of the arbiter partition provided no cell's op-stream is split across arbiters, so physical repartition along cell boundaries is a gauge (the switch is sound; a boundary that sweeps through one arbiter is the falsifier). Deps: rosette-game-protocol.

From the checkout root:

```sh
tools/test-system causal-frontier
```

## Exported implementation and tests

- [src/causal-frontier.lisp](src/causal-frontier.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
