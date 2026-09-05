# blc

## blc

John Tromp's Binary Lambda Calculus: the encoding that makes Minimum-Description-Length literal -- a de-Bruijn lambda term gets an EXACT bit-length. Terms are (:var n)/(:lam body)/(:app f a). blc-encode lowers a term to a bitstring (variable index n -> 1^(n+1) 0; abstraction -> 00 enc(body); application -> 01 enc(f) enc(g)); blc-length gives the bit count without building the string; blc-decode parses bits back to a term (round-trip). Ships the S/K/I combinators and Church numerals as terms so substrate laws can be MEASURED in bits.

From the checkout root:

```sh
tools/test-system blc
```

## Exported implementation and tests

- [src/blc2.lisp](src/blc2.lisp)
- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [src/sharing.lisp](src/sharing.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
