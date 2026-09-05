# distribution-compiler

## distribution-compiler

Compile policy-defined Rosette system closures into standalone source repositories with checked namespaces, licenses, provenance, and reproducible identities.

From the checkout root:

```sh
tools/test-system distribution-compiler
```

## Exported implementation and tests

- [src/distribution-compiler.lisp](src/distribution-compiler.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
