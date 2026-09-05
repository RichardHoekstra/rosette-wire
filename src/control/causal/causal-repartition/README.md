# causal-repartition

## causal-repartition

The physical-layer rebalancer that flows under the logical gravity-well tree: which machine runs which piece, rebalanced on load, invisible to the player and (because the logical outcome is partition-invariant) a gauge on the world. Load is measured as CAUSAL-EVENT-RATE (adjudications/time = the Delta_der residue two objects force), not occupancy. The min-cut cost model: net gain of a cut S|T at latency-ratio lambda = min(internal(S),internal(T)) - (lambda-1)*cut(S,T) -- parallelism won minus the excess price of severed edges. This is a LOCATED PHASE TRANSITION: a barbell (dense clusters, thin bridge) splits, a dense clique does not, the boundary is bridge = internal/(lambda-1), and lambda is its sharpness. Measurement is local per seam. A two-threshold HYSTERESIS controller turns the sharp transition into a discrete, HLC-stamped, replayable SWITCH (split above theta-hi, merge below theta-lo, hold between) that provably collapses the flapping a single threshold suffers. Deps: rosette-causal-frontier.

From the checkout root:

```sh
tools/test-system causal-repartition
```

## Exported implementation and tests

- [src/causal-repartition.lisp](src/causal-repartition.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
