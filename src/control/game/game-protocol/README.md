# game-protocol

## game-protocol

The deterministic-replay contract every Rosette game backend is an instance of. Five generic functions -- game-advance (one tick under one opaque input), game-hash (deterministic state fingerprint), game-tick (integer tick), and the game-capture/game-restore snapshot pair that abstracts over the functional-vs-mutating gauge -- plus one portable verifier: run-game records a captured initial world, per-tick inputs, and a fingerprint trace; replay-p restores and re-runs, proving identical fingerprints. A backend conforms by supplying methods; capture/restore default to identity so a purely functional stepper is conformant with no extra code. Zero dependencies; this is the gauge-invariant that rosette-minecraft-clone and rosette-game-engine each stopped re-implementing.

From the checkout root:

```sh
tools/test-system game-protocol
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/protocol.lisp](src/protocol.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
