# Rosette Wire

Rosette is a language and runtime for building typed Components, composing
them into bounded programs, and checking their execution evidence.
A Component declares its Ports, effects, capabilities, and verifiers; a
Composition connects those contracts into an executable workflow.

The goal is to make powerful computation useful to scientists, engineers, and
the agents working with them: understandable workflows, explicit assumptions,
and results that can be independently checked. Eshkol is an independent
execution backend, Moonlab a quantum laboratory, and Rose the developer
workbench around the shared contracts.

This preview is a readable Common Lisp/ASDF distribution running on **SBCL**.
It integrates with Eshkol through an optional execution adapter; it is not yet
an Eshkol-native application framework. No model or external service is needed
for the default development loop.

## Start here

Requirements: SBCL with ASDF, GNU Make, and Bash.

```sh
sbcl --script examples/verified-program.lisp run /tmp/rosette-demo
sbcl --script examples/verified-program.lisp replay /tmp/rosette-demo
```

The example executes a real program, saves its Composition and receipt, and
reruns its verifier in a fresh process. The [walkthrough](docs/quickstart.md)
also demonstrates a deliberately wrong answer being refused.

## Build together

Start with the example's explicit handler and verifier. Keep implementations
in their owning projects and exchange contracts and evidence through Ports.
Read the [interoperability guide](docs/interop.md) for the current
[Eshkol](https://github.com/tsotchke/eshkol) and
[Moonlab](https://github.com/tsotchke/moonlab) boundaries.

```sh
make contributor-check
```

PRs are welcome at [rosette-wire](https://github.com/RichardHoekstra/rosette-wire). [CONTRIBUTING.md](CONTRIBUTING.md) covers
ordinary source changes, new adapters, tests, and the maintainer-assisted path
for generated files. The [system reference](docs/systems.md) links the exact
81 exported ASDF systems and their readable source and tests.

## Release boundary

This is pre-1.0 software. The source repository is a compiled Apache-2.0
distribution with stable public names; optimization artifacts do not replace
the editable source contract. Shared improvements are integrated upstream and
regenerated into every affected export. See [architecture](docs/architecture.md),
[provenance](release/PROVENANCE.md), and [security reporting](SECURITY.md).

`make release-check` checks the exact sealed export, including the full test
suite, independent tree identity, reproducible archives, and a filesystem-isolated
clean room. It intentionally rejects a modified contributor checkout.
