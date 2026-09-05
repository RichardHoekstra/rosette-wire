# exact-linear-subquotient

## exact-linear-subquotient

Zero-dependency exact rational linear algebra for H=ker(A)/im(B): immutable carriers, quotient representatives and class coordinates, executable projection/lift/retraction receipts, and map descent with separately located cycle- and relation-preservation residuals. Row-list matrices act on columns; all arithmetic is exact Common Lisp rational arithmetic.

From the checkout root:

```sh
tools/test-system exact-linear-subquotient
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
