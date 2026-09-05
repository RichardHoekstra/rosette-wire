# net-frame

## net-frame

Network data-path kernel: Ethernet II + IPv4 header build/parse, the IPv4 one's-complement (internet) checksum, and CRC32 (Ethernet FCS), with frame<->deframe round-trip.

From the checkout root:

```sh
tools/test-system net-frame
```

## Exported implementation and tests

- [src/core.lisp](src/core.lisp)
- [src/package.lisp](src/package.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
