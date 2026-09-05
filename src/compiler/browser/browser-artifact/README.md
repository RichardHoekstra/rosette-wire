# browser-artifact

## browser-artifact

Deterministic, policy-audited assembly of self-contained browser artifacts.

From the checkout root:

```sh
tools/test-system browser-artifact
```

## Exported implementation and tests

- [src/browser-artifact.lisp](src/browser-artifact.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
