# sharing-reduction

## sharing-reduction

Interaction-net optimal reduction with replayable per-term schedule-gauge certificates: maximal sharing tames naive duplication, while local strongly-confluent rewrites certify sequential and parallel normal-form agreement.

From the checkout root:

```sh
tools/test-system sharing-reduction
```

## Exported implementation and tests

- [src/church.lisp](src/church.lisp)
- [src/lambda.lisp](src/lambda.lisp)
- [src/net.lisp](src/net.lisp)
- [src/package.lisp](src/package.lisp)
- [src/readback.lisp](src/readback.lisp)
- [src/reduce.lisp](src/reduce.lisp)
- [src/rules.lisp](src/rules.lisp)
- [src/verdict.lisp](src/verdict.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
