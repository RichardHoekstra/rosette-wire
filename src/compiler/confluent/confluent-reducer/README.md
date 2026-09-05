# confluent-reducer

## confluent-reducer

A certified-parallel functional reducer over rosette-core-term where PARALLELISM IS A GAUGE, not a hope: every reduction emits its own proof.  It reduces the accumulator/op fragment of a core-term (a let-chain of `(+ cell lit)` and `lit` updates on named cells), extracts the per-cell op-stream, and uses rosette-confluence-frontier to partition the redexes into the CONFLUENT set (order-independent runs of commuting ops -> fire as a parallel batch, any schedule reaches the same partial result) and the NON-CONFLUENT frontier (a forgetting `:set` reset that does not commute -> the irreducibly-serial residue).  The parallel schedule is sealed as a gauge by rosette-causal-frontier PARTITION-INVARIANCE (moving cells across arbiters is invariant IFF no cell's op-stream is split), and rosette-causal-repartition's min-cut cost model quantifies the cross-cell parallel width (the located split/no-split phase transition at critical-bridge).  It inherits rosette-core-term's coincidence: the reduced value == core-term's own EVAL == its normalizer's normal form, and the whole run is sealed into a re-runnable rosette-proof-witness certificate.  HONEST SCOPE: `parallel` means PROVABLY ORDER-INDEPENDENT (a gauge), not OS-threaded execution; the classification is exact over free permutations of the one-cell :add/:set algebra (a causally-final forgetting :set reads as a frontier -- the rosette-forgetting-frontier subtlety); general pure core-term redexes are already fully confluent (empty non-confluent frontier).

From the checkout root:

```sh
tools/test-system confluent-reducer
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
