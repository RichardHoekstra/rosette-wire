# expression-core

## expression-core

Dependency-free closed-form expression AST, builders, substitution, and structural analysis.

From the checkout root:

```sh
tools/test-system expression-core
```

## Exported implementation and tests

- [src/analysis.lisp](src/analysis.lisp)
- [src/ast.lisp](src/ast.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
