# unification

## unification

First-order logic programming core: terms, substitutions, Robinson unification with occurs-check (most general unifier), SLD resolution over a Horn clause knowledge base with fresh-variable renaming, and the DUAL operation -- Plotkin-Reynolds anti-unification (least general generalization) with one-sided matching, so UNIFY (common instance / meet) and ANTI-UNIFY (common generalization / join) are the two operations of the subsumption lattice. The symbolic half of the neurosymbolic loop (Prolog's engine as a zero-dependency kernel).

From the checkout root:

```sh
tools/test-system unification
```

## Exported implementation and tests

- [src/anti-unify.lisp](src/anti-unify.lisp)
- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
