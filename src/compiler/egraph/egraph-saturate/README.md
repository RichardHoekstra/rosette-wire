# egraph-saturate

## egraph-saturate

Certificate-gated equality saturation over content-addressed Forms: dedup computations by value, mint re-runnable equivalence theorems, meter DAG op-sharing.

From the checkout root:

```sh
tools/test-system egraph-saturate
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/saturate.lisp](src/saturate.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
