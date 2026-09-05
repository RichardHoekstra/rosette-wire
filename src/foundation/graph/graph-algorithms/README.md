# graph-algorithms

## graph-algorithms

Classic graph algorithms (topological sort, connected components, MST, max-flow/min-cut, centrality) over rosette-graph-core CSR adjacency.

From the checkout root:

```sh
tools/test-system graph-algorithms
```

## Exported implementation and tests

- [src/centrality.lisp](src/centrality.lisp)
- [src/components.lisp](src/components.lisp)
- [src/max-flow.lisp](src/max-flow.lisp)
- [src/mst.lisp](src/mst.lisp)
- [src/package.lisp](src/package.lisp)
- [src/topological-sort.lisp](src/topological-sort.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
