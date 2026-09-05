# json

## json

Dependency-free JSON codec: parse a JSON string into Lisp data (alists/lists/strings/numbers/booleans/null) and encode Lisp data back to JSON, with round-trip and clear errors on malformed input.

From the checkout root:

```sh
tools/test-system json
```

## Exported implementation and tests

- [src/encode.lisp](src/encode.lisp)
- [src/package.lisp](src/package.lisp)
- [src/parse.lisp](src/parse.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
