# union-find-core

## union-find-core

Disjoint-set forest (union-find) with union by rank, path compression, growable element set, and component enumeration.

From the checkout root:

```sh
tools/test-system union-find-core
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
