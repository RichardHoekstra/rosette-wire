# process-engineering

## process-engineering

Small law-tested process-engineering kernel for material streams, native stoichiometric reactions, unit-operation carriers, heat ledgers, and mass-balance residuals.

From the checkout root:

```sh
tools/test-system process-engineering
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
