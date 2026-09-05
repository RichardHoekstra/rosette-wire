# toml

## toml

Dependency-free strict TOML subset for scientific case configuration: tables, dotted keys, strings, booleans, finite numbers, and homogeneous arrays, with located parse errors and deterministic writing.

From the checkout root:

```sh
tools/test-system toml
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/toml.lisp](src/toml.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
