# program-ir

## program-ir

Minimal homoiconic program AST, conversion, walking, and canonical equality primitives.

From the checkout root:

```sh
tools/test-system program-ir
```

## Exported implementation and tests

- [src/ast.lisp](src/ast.lisp)
- [src/convert.lisp](src/convert.lisp)
- [src/equality.lisp](src/equality.lisp)
- [src/package.lisp](src/package.lisp)
- [src/walk.lisp](src/walk.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
