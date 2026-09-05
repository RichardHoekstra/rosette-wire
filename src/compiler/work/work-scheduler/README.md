# work-scheduler

## work-scheduler

Read/write footprint and dependency-frontier analysis for low-overhead safe parallel work.

From the checkout root:

```sh
tools/test-system work-scheduler
```

## Exported implementation and tests

- [src/actions.lisp](src/actions.lisp)
- [src/certificates.lisp](src/certificates.lisp)
- [src/core.lisp](src/core.lisp)
- [src/effects.lisp](src/effects.lisp)
- [src/package.lisp](src/package.lisp)
- [src/planning.lisp](src/planning.lisp)
- [src/scheduler-certificate-internals.lisp](src/scheduler-certificate-internals.lisp)
- [src/scheduler-effect-internals.lisp](src/scheduler-effect-internals.lisp)
- [src/scheduler-internals.lisp](src/scheduler-internals.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
