# confluence-frontier

## confluence-frontier

The boundary between order-free (symbolic / verifiable) and measure-requiring (neuro / measurement) computation. A local rewrite system is symbolic exactly where it is confluent: concurrent redexes commute, so the normal form is order-independent -- no consensus and no tiebreak, a verifier decides it for free. It becomes neuro exactly at a non-confluent redex whose outcome depends on order: no local fact selects the branch, so an external MEASURE (Born rule / learned amplitude / sampler) must supply it. measurement-needed-p is that frontier. The neurosymbolic answer read off the confluence structure: confluent => symbolic (unique-result, verify-only); non-confluent => neuro (resolve-with-measure). Honest subtlety: order-freedom has TWO sources -- commuting (clean) and forgetting (a :set reset that erases the branch distinction, a projective measurement) -- so confluence is SUFFICIENT but not NECESSARY for tiebreak-freedom (converse witnessed). Reuses rosette-causal-frontier's op algebra (does not re-implement semantics). Deps: rosette-causal-frontier. Composes with rosette-code-holography (curvature=confluence) and rosette-cognitive-tape (trace as confluence object).

From the checkout root:

```sh
tools/test-system confluence-frontier
```

## Exported implementation and tests

- [src/confluence-frontier.lisp](src/confluence-frontier.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
