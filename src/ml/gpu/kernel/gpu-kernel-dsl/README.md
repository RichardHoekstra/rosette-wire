# gpu-kernel-dsl

## gpu-kernel-dsl

s-expr CUDA-C kernel DSL: defkernel and pure source codegen.

From the checkout root:

```sh
tools/test-system gpu-kernel-dsl
```

## gpu-kernel-dsl/cpu

Pure-Lisp CPU interpreter for the gpu-kernel-dsl DSL: execute a KERNEL-SPEC with no CUDA.

From the checkout root:

```sh
tools/test-system gpu-kernel-dsl/cpu
```

## Exported implementation and tests

- [src/backend-types.lisp](src/backend-types.lisp)
- [src/bundled-kernels.lisp](src/bundled-kernels.lisp)
- [src/codegen-expr.lisp](src/codegen-expr.lisp)
- [src/codegen.lisp](src/codegen.lisp)
- [src/conditions.lisp](src/conditions.lisp)
- [src/interpret.lisp](src/interpret.lisp)
- [src/launch-base.lisp](src/launch-base.lisp)
- [src/memory-schedule.lisp](src/memory-schedule.lisp)
- [src/online-softmax-moment.lisp](src/online-softmax-moment.lisp)
- [src/package.lisp](src/package.lisp)
- [src/register-fragment.lisp](src/register-fragment.lisp)
- [src/spec.lisp](src/spec.lisp)
- [src/validate.lisp](src/validate.lisp)
- [src/warp-reduction.lisp](src/warp-reduction.lisp)
- [tests/cpu-tests.lisp](tests/cpu-tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
