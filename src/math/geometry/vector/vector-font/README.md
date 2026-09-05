# vector-font

## vector-font

Single-stroke (Hershey roman-simplex) vector font as pure 2-D polyline geometry: char/string -> stroke lists in a normalized baseline frame, with advance/cap-height metrics, scaling, rotation and bounds. Covers printable ASCII 32-126 plus the Greek alphabet keyed by Unicode code point (a literal Greek char renders directly). ZERO dependencies and NO rasterization -- every backend (AA raster stroker, SVG <path>, GPU line-list, pen-plotter) consumes the same gauge-invariant strokes. Glyph data generated from the public-domain Hershey font set (rowmans + greekc).

From the checkout root:

```sh
tools/test-system vector-font
```

## Exported implementation and tests

- [src/font.lisp](src/font.lisp)
- [src/glyphs.lisp](src/glyphs.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
