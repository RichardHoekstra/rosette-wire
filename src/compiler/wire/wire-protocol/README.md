# wire-protocol

## wire-protocol

Bounded byte-stream transport for immutable content-addressed Cells using fixed binary Frames and an incremental multiplexed Wire decoder.

From the checkout root:

```sh
tools/test-system wire-protocol
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/wire-protocol.lisp](src/wire-protocol.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
