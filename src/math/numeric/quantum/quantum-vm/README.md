# quantum-vm

## quantum-vm

Coefficient-generic quantum tape VM with state-free qIR specialization, hyper-dual/adjoint/occurrence-shift AD, matrix-free QGT callbacks, and a bounded data-only circuit value/gradient filter.

From the checkout root:

```sh
tools/test-system quantum-vm
```

## Exported implementation and tests

- [src/adjoint.lisp](src/adjoint.lisp)
- [src/circuit-grad.lisp](src/circuit-grad.lisp)
- [src/gradients.lisp](src/gradients.lisp)
- [src/interpreter.lisp](src/interpreter.lisp)
- [src/observable.lisp](src/observable.lisp)
- [src/package.lisp](src/package.lisp)
- [src/scalars.lisp](src/scalars.lisp)
- [src/specialize.lisp](src/specialize.lisp)
- [src/state.lisp](src/state.lisp)
- [src/verify.lisp](src/verify.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
