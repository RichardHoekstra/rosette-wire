# composition-namespace

## composition-namespace

Project Rosette Compositions into a deterministic Plan 9-style namespace with Wire-backed operation endpoints and a bounded semantic 9P2000 adapter.

From the checkout root:

```sh
tools/test-system composition-namespace
```

## Exported implementation and tests

- [src/composition-namespace.lisp](src/composition-namespace.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
