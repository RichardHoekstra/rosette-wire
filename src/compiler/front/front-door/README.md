# front-door

## front-door

A named, deliberately small front door over the existing core-term spine: one surface program is parsed, represented in ProgramIR, evaluated, lowered to the shared kernel VM, oracle-gated, and sealed in a re-runnable certificate. It also exposes an honest common closed STLC fragment with independently checked core/FinSet-CCC/BLC meanings, an exact integral expression-core bridge, and an optional ProgramIR-to-Eshkol JIT/AOT execution floor.

From the checkout root:

```sh
tools/test-system front-door
```

## front-door/eshkol

Optional ProgramIR-to-Eshkol backend for the validated front-door integer dialect. It emits deterministic Scheme, executes true cache-disabled LLVM JIT and explicit AOT under rosette-isolated-worker ceilings, and compares both results with the direct core evaluator and canonical kernel-VM oracle.

From the checkout root:

```sh
tools/test-system front-door/eshkol
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/eshkol.lisp](src/eshkol.lisp)
- [src/package.lisp](src/package.lisp)
- [src/terms.lisp](src/terms.lisp)
- [tests/eshkol-tests.lisp](tests/eshkol-tests.lisp)
- [tests/fixtures/fake-eshkol.sh](tests/fixtures/fake-eshkol.sh)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
