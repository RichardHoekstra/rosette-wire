# tool-envelope

## tool-envelope

Result envelopes for compiler tools and witnesses with shared metadata validation.

From the checkout root:

```sh
tools/test-system tool-envelope
```

## Exported implementation and tests

- [src/envelope.lisp](src/envelope.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
