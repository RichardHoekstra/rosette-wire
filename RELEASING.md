# Releasing

Releases are human-sealed. Automation builds and checks artifacts but does not
publish them.

1. Start from a freshly compiled export whose `release/VERSION` is final.
2. Run `make release-check`.
3. Review `release/PROVENANCE.md`, `CHANGELOG.md`, and the exact export identity.
4. Commit the generated tree and create the signed tag `v0.1.0-rc.3`.
5. Run `make dist`, then verify the sidecar with
   `cd dist && sha256sum -c rosette-wire-0.1.0-rc.3.tar.gz.sha256`.
6. Publish exactly the checked archive and sidecar; record their digest in the
   release notes.

Changing any tracked byte after step 2 invalidates `release/EXPORT-ID` and
requires recompilation from the canonical source distribution.
