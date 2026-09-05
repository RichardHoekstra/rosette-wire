# lisp-codegen

## lisp-codegen

A compiler from a small integer Lisp (literals, arithmetic, comparison, IF, and recursive function calls) to stack-machine bytecode, plus a reference virtual machine that runs it.  VM values are rosette-lisp-objects tagged fixnums, under which ADD/SUB/MOD are raw word ops, MUL=(a*b)/8, DIV=(a/b)*8, and signed compare is monotonic -- so the SAME bytecode runs unchanged on the native VM (see /native, lowered through rosette-gpu-kernel-dsl's WHILE loop).  The calling convention uses a separate operand stack and call stack: CALL saves (ret-pc, old-fp), RET collapses the frame.  Recursion (factorial, fib, gcd, ackermann, mutual even/odd) compiles and runs.

From the checkout root:

```sh
tools/test-system lisp-codegen
```

## Exported implementation and tests

- [src/codegen.lisp](src/codegen.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
