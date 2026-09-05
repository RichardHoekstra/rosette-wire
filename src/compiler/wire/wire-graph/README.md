# wire-graph

## wire-graph

Immutable typed component descriptors, canonical graph identities, deterministic validation, and bounded Wire execution.

From the checkout root:

```sh
tools/test-system wire-graph
```

## Exported implementation and tests

- [src/canonical-json.lisp](src/canonical-json.lisp)
- [src/cli.lisp](src/cli.lisp)
- [src/decode.lisp](src/decode.lisp)
- [src/graph.lisp](src/graph.lisp)
- [src/package.lisp](src/package.lisp)
- [src/runtime.lisp](src/runtime.lisp)
- [src/semantic-aliases.lisp](src/semantic-aliases.lisp)
- [src/wire-graph.lisp](src/wire-graph.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
