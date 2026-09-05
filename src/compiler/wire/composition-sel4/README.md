# composition-sel4

## composition-sel4

Lower Rosette Component capabilities and explicit service/data-flow edges into a content-addressed seL4/Microkit authority description with deadlock and information-flow verification.

From the checkout root:

```sh
tools/test-system composition-sel4
```

## Exported implementation and tests

- [src/composition-sel4.lisp](src/composition-sel4.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
