# raster-core

## raster-core

Primitive RGB framebuffer, pixel blending, line rasterization, and PPM I/O.

From the checkout root:

```sh
tools/test-system raster-core
```

## Exported implementation and tests

- [src/conditions.lisp](src/conditions.lisp)
- [src/frame.lisp](src/frame.lisp)
- [src/lines.lisp](src/lines.lisp)
- [src/package.lisp](src/package.lisp)
- [src/ppm.lisp](src/ppm.lisp)
- [src/types.lisp](src/types.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
