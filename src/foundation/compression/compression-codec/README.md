# compression-codec

## compression-codec

Lossless compression encoders: canonical Huffman coding, LZ77, a DEFLATE (RFC 1951) encoder producing rosette-inflate-decodable output, and a byte-oriented range/arithmetic coder.

From the checkout root:

```sh
tools/test-system compression-codec
```

## Exported implementation and tests

- [src/bitstream.lisp](src/bitstream.lisp)
- [src/deflate.lisp](src/deflate.lisp)
- [src/huffman.lisp](src/huffman.lisp)
- [src/lz77.lisp](src/lz77.lisp)
- [src/package.lisp](src/package.lisp)
- [src/range-coder.lisp](src/range-coder.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
