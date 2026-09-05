# hott-core

## hott-core

Homotopy type theory vocabulary: types, paths, equivalences, dependent products/sums, quotients, and truncation levels.

From the checkout root:

```sh
tools/test-system hott-core
```

## Exported implementation and tests

- [src/circle-interval.lisp](src/circle-interval.lisp)
- [src/core.lisp](src/core.lisp)
- [src/dependent-types.lisp](src/dependent-types.lisp)
- [src/equivalence-fibers.lisp](src/equivalence-fibers.lisp)
- [src/equivalences-pullbacks.lisp](src/equivalences-pullbacks.lisp)
- [src/equivalences.lisp](src/equivalences.lisp)
- [src/families.lisp](src/families.lisp)
- [src/hit-pushouts.lisp](src/hit-pushouts.lisp)
- [src/package.lisp](src/package.lisp)
- [src/paths.lisp](src/paths.lisp)
- [src/pointed-constructions.lisp](src/pointed-constructions.lisp)
- [src/pointed-equivalences.lisp](src/pointed-equivalences.lisp)
- [src/pointed-products.lisp](src/pointed-products.lisp)
- [src/quotients-truncation.lisp](src/quotients-truncation.lisp)
- [src/suspension-spheres.lisp](src/suspension-spheres.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
