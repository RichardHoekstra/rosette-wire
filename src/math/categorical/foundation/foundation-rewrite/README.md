# foundation-rewrite

## foundation-rewrite

Rewrite kernels for foundation categorical: normal forms, TLC beta-reduction, confluence, and DAG-CSE axiom ranking.

From the checkout root:

```sh
tools/test-system foundation-rewrite
```

## Exported implementation and tests

- [src/church-rosser.lisp](src/church-rosser.lisp)
- [src/conditions.lisp](src/conditions.lisp)
- [src/normal-form.lisp](src/normal-form.lisp)
- [src/optimal-axiom.lisp](src/optimal-axiom.lisp)
- [src/package.lisp](src/package.lisp)
- [src/util.lisp](src/util.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
