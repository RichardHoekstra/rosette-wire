# chain-complex

## chain-complex

The abstract (co)chain complex: exact d^2=0, field Betti numbers, integer torsion, relative coordinate quotients, and Euler-Poincare. Exact rational H^n is promoted from a dimension to ker(d_(n+1)^T)/im(d_n^T) with cocycle representatives and class coordinates. Exact per-degree chain maps carry replayable d f = f d receipts and induce certified contravariant cohomology maps by subquotient descent; identity and composition are verifier-backed. Simplicial/graph/Cech builders make this the common topological carrier.

From the checkout root:

```sh
tools/test-system chain-complex
```

## Exported implementation and tests

- [src/builders.lisp](src/builders.lisp)
- [src/chain-map.lisp](src/chain-map.lisp)
- [src/complex.lisp](src/complex.lisp)
- [src/cup.lisp](src/cup.lisp)
- [src/homology.lisp](src/homology.lisp)
- [src/package.lisp](src/package.lisp)
- [src/rank.lisp](src/rank.lisp)
- [src/rational-cohomology.lisp](src/rational-cohomology.lisp)
- [src/relative.lisp](src/relative.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
