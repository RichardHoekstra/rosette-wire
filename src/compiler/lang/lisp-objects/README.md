# lisp-objects

## lisp-objects

The value representation for a from-scratch Lisp runtime: low-3-bit lowtags on a flat machine-word heap (fixnum/cons/char/singleton/closure), a precise self-describing tracer, a copying (compacting) collector, and flat-closure objects. Every word says pointer-or-not, so the heap is exactly traceable -- no conservative scanning -- and cycles are handled by marking. The substrate a tagged-value JIT backend (rosette-gpu-kernel-dsl native path) must emit and a GC must trace, zero deps.

From the checkout root:

```sh
tools/test-system lisp-objects
```

## Exported implementation and tests

- [src/objects.lisp](src/objects.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
