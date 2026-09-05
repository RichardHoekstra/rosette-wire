# Governance

Rosette is maintainer-led and developed in public through issues and reviewed
change proposals. Maintainers decide scope, merge accepted contributions into
the canonical source, and emit the next auditable distribution.

Protocol and verifier changes require executable compatibility evidence and
negative controls. Rose and model-backed clients never have authority to declare
a result valid; only deterministic Rosette validation and verification do.

Commit, tag, push, deployment, and publication are distinct operations. A public
release requires an explicit human seal after `make release-check` passes for
the exact export identity. Automated systems may build evidence and artifacts;
they may not manufacture that seal.
