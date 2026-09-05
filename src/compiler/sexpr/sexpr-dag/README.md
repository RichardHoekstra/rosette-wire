# sexpr-dag

## sexpr-dag

Hash-consed s-expression DAGs with CSE metrics and source provenance.

From the checkout root:

```sh
tools/test-system sexpr-dag
```

## Exported implementation and tests

- [src/analysis-concepts.lisp](src/analysis-concepts.lisp)
- [src/analysis-extraction.lisp](src/analysis-extraction.lisp)
- [src/analysis-hotspots.lisp](src/analysis-hotspots.lisp)
- [src/analysis-isomorphism.lisp](src/analysis-isomorphism.lisp)
- [src/analysis-printers.lisp](src/analysis-printers.lisp)
- [src/analysis-profiles.lisp](src/analysis-profiles.lisp)
- [src/analysis-shape-binding-predicates.lisp](src/analysis-shape-binding-predicates.lisp)
- [src/analysis-shape-domain-predicates.lisp](src/analysis-shape-domain-predicates.lisp)
- [src/analysis-shape-predicates.lisp](src/analysis-shape-predicates.lisp)
- [src/analysis-shape-roles.lisp](src/analysis-shape-roles.lisp)
- [src/analysis-shape.lisp](src/analysis-shape.lisp)
- [src/analysis-summary.lisp](src/analysis-summary.lisp)
- [src/analysis.lisp](src/analysis.lisp)
- [src/dag.lisp](src/dag.lisp)
- [src/package.lisp](src/package.lisp)
- [src/reader.lisp](src/reader.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
