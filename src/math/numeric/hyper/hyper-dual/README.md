# hyper-dual

## hyper-dual

Hyper-dual numbers plus order-3/4 and arbitrary-order univariate jets for exact forward-mode autodiff, with executable scalar-tolerance certificates that transport one absolute/relative budget across every derivative order using explicit input-scale units.

From the checkout root:

```sh
tools/test-system hyper-dual
```

## Exported implementation and tests

- [src/black-scholes.lisp](src/black-scholes.lisp)
- [src/exact-jet.lisp](src/exact-jet.lisp)
- [src/grad-hessian.lisp](src/grad-hessian.lisp)
- [src/hyper-dual.lisp](src/hyper-dual.lisp)
- [src/jet-n.lisp](src/jet-n.lisp)
- [src/jet3.lisp](src/jet3.lisp)
- [src/jet4.lisp](src/jet4.lisp)
- [src/mvjet.lisp](src/mvjet.lisp)
- [src/package.lisp](src/package.lisp)
- [src/tolerance.lisp](src/tolerance.lisp)
- [src/util.lisp](src/util.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
