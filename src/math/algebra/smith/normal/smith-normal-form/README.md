# smith-normal-form

## smith-normal-form

Exact integer Smith normal form (D = U A V, U/V unimodular, invariant factors d_1|d_2|...) and integer homology WITH torsion -- the torsion the rational/Betti matrix-rank homology misses (Klein bottle H_1 = Z (+) Z/2).

From the checkout root:

```sh
tools/test-system smith-normal-form
```

## Exported implementation and tests

- [src/homology.lisp](src/homology.lisp)
- [src/package.lisp](src/package.lisp)
- [src/smith.lisp](src/smith.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
