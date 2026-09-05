# sel4-cap

## sel4-cap

Typed raw seL4 endpoint-capability call contract and seed conformance gate.

From the checkout root:

```sh
tools/test-system sel4-cap
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/sel4-cap.lisp](src/sel4-cap.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
