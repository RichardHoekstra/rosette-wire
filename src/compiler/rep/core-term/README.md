# core-term

## core-term

ONE typed first-order core term language (integers + let + if + arithmetic + bounded recursion) carrying THREE coincidences on the SAME term: (1) EVAL -- a direct interpreter; (2) PROVE -- a normalizer whose reduction EMITS a proof-carrier-core node per step, sealed into a re-runnable proof-witness certificate, so NORMALIZING PRODUCES THE CERTIFICATE (definitional equality decided by normal-form identity); (3) COMPILE -- the same term lowered (let-substituted) to rosette-lisp-codegen integer Lisp, assembled to bytecode, run on the rosette-gpu-kernel-dsl kernel-spec VM through the CPU oracle (interpret-kernel-spec), BIT-EXACT against the evaluator.  eval and prove are DERIVED here; the silicon lowering is BRIDGED through the proven rosette-lisp-codegen -> kernel-IR path.  A corrupted lowering is caught by the bit-check; two beta-equal terms share a normal form and two in-equal terms do not; a forged certificate fails its re-run.

From the checkout root:

```sh
tools/test-system core-term
```

## Exported implementation and tests

- [src/lower.lisp](src/lower.lisp)
- [src/normalize.lisp](src/normalize.lisp)
- [src/package.lisp](src/package.lisp)
- [src/reduce.lisp](src/reduce.lisp)
- [src/selfeval-f.lisp](src/selfeval-f.lisp)
- [src/selfeval.lisp](src/selfeval.lisp)
- [src/term.lisp](src/term.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
