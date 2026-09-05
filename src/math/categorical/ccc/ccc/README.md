# ccc

## ccc

A cartesian closed category (FinSet -- finite sets and functions) and the Lambek correspondence to the simply-typed lambda-calculus: the categorical THIRD leg of Curry-Howard-Lambek. Objects/morphisms with identity + composition (associativity + unit laws), a terminal object (unit type), binary products (projections + pairing with the mediating-morphism universal property), and exponentials B^A with eval + currying. The headline law is the exponential ADJUNCTION Hom(A x B, C) ~ Hom(A, C^B): curry/uncurry are a NATURAL bijection -- this IS currying, and a CCC is the simply-typed lambda-calculus (Lambek 1970 / Lawvere). beta and eta become categorical equations (eval . (curry f x id) = f ; curry eval = id). A de-Bruijn STLC denotes compositionally into FinSet and a small term round-trips through its morphism (term -> morphism -> value -> normal form), with denotation invariant under beta/eta reduction.

From the checkout root:

```sh
tools/test-system ccc
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
