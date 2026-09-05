# isolated-worker

## isolated-worker

Bounded subprocess supervisor with argv-only launch, resource ceilings, compact receipt parsing, and privacy-safe output identities.

From the checkout root:

```sh
tools/test-system isolated-worker
```

## Exported implementation and tests

- [src/package.lisp](src/package.lisp)
- [src/worker.lisp](src/worker.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
