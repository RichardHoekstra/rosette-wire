# Release model

The repository uses semantic versions. Schema identifiers and receipt formats
are versioned independently; a package version does not silently redefine an
existing schema URI.

A release candidate is acceptable only when `make release-check` passes from
the exact exported tree. The release archive has one top-level directory named
`rosette-wire-0.1.0-rc.3`, deterministic metadata, and a portable SHA-256 sidecar.

Tags and public releases are human-sealed operations. CI may build and retain
the exact artifacts for a tag, but it does not decide that a candidate is valid
or publish a release on its own.
