# claim-referee

## claim-referee

Generic falsifiable-claim referee: model an extraordinary claim as
data (name, claimed value or inequality, tolerance, measurement thunk), run the
measurement, and emit a structured verdict -- :confirmed / :refuted / :partial /
:untestable -- carrying the claimed value, the measured value, the ratio/margin,
and a human-readable reason.  Composite claims decompose into sub-claims so the
verdict separates which parts hold (the fusion 'ignition != Q>1 != plant-positive'
pattern).  Skeptical by default: a measurement that errors or is missing is
:untestable, never :confirmed.  The honest-accounting moat as a reusable kernel.

From the checkout root:

```sh
tools/test-system claim-referee
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
