# quantum-ir

## quantum-ir

Proof-carrying substructural quantum ProgramIR: every SSA value has a replayable canonical usage profile, measurement emits unrestricted classical data plus linear quantum state, explicit COPY/DISCARD boundaries are type-licensed, and deterministic semantic tapes preserve the effects.

From the checkout root:

```sh
tools/test-system quantum-ir
```

## Exported implementation and tests

- [src/certification.lisp](src/certification.lisp)
- [src/elaborate.lisp](src/elaborate.lisp)
- [src/ir.lisp](src/ir.lisp)
- [src/package.lisp](src/package.lisp)
- [src/tape.lisp](src/tape.lisp)
- [src/typecheck.lisp](src/typecheck.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
