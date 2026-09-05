# symmetric-crypto

## symmetric-crypto

Symmetric cryptography: AES (ECB/CTR/GCM), ChaCha20-Poly1305, HMAC-SHA256, HKDF -- vector-verified against FIPS-197/RFC-8439/RFC-4231/RFC-5869.

From the checkout root:

```sh
tools/test-system symmetric-crypto
```

## Exported implementation and tests

- [src/aes-ctr.lisp](src/aes-ctr.lisp)
- [src/aes.lisp](src/aes.lisp)
- [src/bytes.lisp](src/bytes.lisp)
- [src/chacha20-poly1305.lisp](src/chacha20-poly1305.lisp)
- [src/chacha20.lisp](src/chacha20.lisp)
- [src/gcm.lisp](src/gcm.lisp)
- [src/hkdf.lisp](src/hkdf.lisp)
- [src/hmac.lisp](src/hmac.lisp)
- [src/package.lisp](src/package.lisp)
- [src/poly1305.lisp](src/poly1305.lisp)
- [src/sha256.lisp](src/sha256.lisp)
- [tests/tests.lisp](tests/tests.lisp)

The ASDF definitions list the exact included components and dependency
boundaries. Source docstrings and executable tests specify the detailed API.

## License

Apache-2.0. See [LICENSE](LICENSE).
