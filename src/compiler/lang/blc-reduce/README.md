# blc-reduce

## blc-reduce

BLC EXECUTION (rung 1): a lazy combinator-graph reducer for rosette-blc terms -- the make-it-run side, paired with rosette-blc's measure-it-in-bits side. A faithful port of John Tromp's gblc.c ION machine: Kiselyov bracket abstraction (lambda->combinators S/K/I/B/C/R/T/D/M/Y/F/:) + lazy left-spine reduction with in-place redex update (sharing) + a Cheney copying GC. API: lambda->combinators (bracket abstraction), reduce-whnf, reduce-to-normal-form, reduce-church / reduce-bool (oracle decoders), combinators->term readback, last-step-count. Correctness is oracle-gated against ground-truth arithmetic/logic (Church ADD/MUL/POW/SUCC, S/K/I identities, AND/OR/NOT). rosette-sharing-reduction is rung 4 (interaction-net optimal / GPU-ready reduction); this is rung 1, the combinator graph reducer.

From the checkout root:

```sh
tools/test-system blc-reduce
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
