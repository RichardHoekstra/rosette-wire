# sel4-verify

## sel4-verify

An seL4 / Microkit system-description referee: OS correctness as a computable obstruction. A synchronous protected-call graph deadlocks iff it has a directed cycle; it is deadlock-free iff a global progress potential exists. Information-flow security has the same monotone-potential shape: security labels must not drop along flows. Parses system descriptions, derives protected-call edges, certifies deadlock-freedom and information-flow security, and reports the undirected b1 from rosette-chain-complex as a homological shadow only.

From the checkout root:

```sh
tools/test-system sel4-verify
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/verify.lisp](src/verify.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
