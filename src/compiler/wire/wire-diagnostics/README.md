# wire-diagnostics

## wire-diagnostics

Replayable diagnostic bundles for Eshkol execution-floor and Moonlab public-ABI campaigns.

From the checkout root:

```sh
tools/test-system wire-diagnostics
```

## Exported implementation and tests

- [corpus/eshkol-no-stdlib-aot-link.json](corpus/eshkol-no-stdlib-aot-link.json)
- [corpus/manifest.json](corpus/manifest.json)
- [corpus/moonlab-qgt-nband-sign.json](corpus/moonlab-qgt-nband-sign.json)
- [src/cross-boundary.lisp](src/cross-boundary.lisp)
- [src/generated-campaigns.lisp](src/generated-campaigns.lisp)
- [src/moonlab-diagnostics.lisp](src/moonlab-diagnostics.lisp)
- [src/moonlab-surfaces.lisp](src/moonlab-surfaces.lisp)
- [src/package.lisp](src/package.lisp)
- [src/wire-adapters.lisp](src/wire-adapters.lisp)
- [src/wire-diagnostics.lisp](src/wire-diagnostics.lisp)
- [tests/tests.lisp](tests/tests.lisp)
- [tools/moonlab-abi-probe.c](tools/moonlab-abi-probe.c)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
