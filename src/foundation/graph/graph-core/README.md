# graph-core

## graph-core

CSR adjacency primitive with BFS, DFS, SCC, and edge-attribute storage.

From the checkout root:

```sh
tools/test-system graph-core
```

## Exported implementation and tests

- [src/csr.lisp](src/csr.lisp)
- [src/package.lisp](src/package.lisp)
- [src/scc.lisp](src/scc.lisp)
- [src/traversal.lisp](src/traversal.lisp)
- [tests/package.lisp](tests/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
