# scene-ir

## scene-ir

One homoiconic property-tree s-expr that is simultaneously the fb_scene display list, the compositor layer list, and the layout target: display-list IR + executor over rosette-raster-core, a z-ordered SRC_OVER layer compositor (the promoted seL4 fb_blit), a single-pass block/flow layout lowering, and an fb_scene.txt emitter that feeds the existing freestanding seL4 renderer byte-exact.

From the checkout root:

```sh
tools/test-system scene-ir
```

## Exported implementation and tests

- [src/fbscene.lisp](src/fbscene.lisp)
- [src/ir.lisp](src/ir.lisp)
- [src/layout.lisp](src/layout.lisp)
- [src/ops.lisp](src/ops.lisp)
- [src/package.lisp](src/package.lisp)
- [src/render.lisp](src/render.lisp)
- [src/xform.lisp](src/xform.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
