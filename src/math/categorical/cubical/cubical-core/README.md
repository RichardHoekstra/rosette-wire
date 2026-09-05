# cubical-core

## cubical-core

A minimal computational cubical kernel over rosette-hott-core: a primitive interval I (De Morgan algebra), paths as functions I->A (endpoints reduce definitionally), Kan operations transp/hcomp, and COMPUTATIONAL univalence -- transp (ua e) x reduces to (e x) by structural recursion on the Glue type-line, the beta-rule that is STUCK in book-HoTT.

From the checkout root:

```sh
tools/test-system cubical-core
```

## Exported implementation and tests

- [src/circle-hspace.lisp](src/circle-hspace.lisp)
- [src/circle.lisp](src/circle.lisp)
- [src/connections.lisp](src/connections.lisp)
- [src/cyclic-homotopy.lisp](src/cyclic-homotopy.lisp)
- [src/dependent.lisp](src/dependent.lisp)
- [src/examples.lisp](src/examples.lisp)
- [src/face-lattice.lisp](src/face-lattice.lisp)
- [src/flattening.lisp](src/flattening.lisp)
- [src/glue.lisp](src/glue.lisp)
- [src/homotopy-groups.lisp](src/homotopy-groups.lisp)
- [src/hopf-construction.lisp](src/hopf-construction.lisp)
- [src/hopf.lisp](src/hopf.lisp)
- [src/hspace-coherence.lisp](src/hspace-coherence.lisp)
- [src/interval.lisp](src/interval.lisp)
- [src/join.lisp](src/join.lisp)
- [src/kan.lisp](src/kan.lisp)
- [src/moore-space.lisp](src/moore-space.lisp)
- [src/package.lisp](src/package.lisp)
- [src/pathp.lisp](src/pathp.lisp)
- [src/paths.lisp](src/paths.lisp)
- [src/pi4-encode.lisp](src/pi4-encode.lisp)
- [src/pi4-equiv.lisp](src/pi4-equiv.lisp)
- [src/pi4-s3.lisp](src/pi4-s3.lisp)
- [src/sphere2.lisp](src/sphere2.lisp)
- [src/squares.lisp](src/squares.lisp)
- [src/suspension.lisp](src/suspension.lisp)
- [src/truncation.lisp](src/truncation.lisp)
- [src/type-formers.lisp](src/type-formers.lisp)
- [src/whitehead.lisp](src/whitehead.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
