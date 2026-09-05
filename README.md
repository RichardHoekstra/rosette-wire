# Rosette Wire

**Give computations a contract that other tools can execute and check.**

Rosette is a component protocol and runtime toolkit for building scientific
software. Describe an operation's inputs, outputs, effects, capabilities, and
required evidence; connect operations into a workflow; then execute it and
verify the resulting receipt.

Use Rosette when you're **building a tool, adapter, or execution backend** that
needs to cooperate with other implementations. For an experiment workbench and
visual inspection, start with
[Rose Workbench](https://github.com/RichardHoekstra/rose-workbench).

## The contract

A **Component** declares what an operation accepts, produces, and needs.
A **Composition** connects components through typed ports and declares the
execution bounds. A **receipt** records what happened. Registered verifiers
check the required evidence against that composition.

These contracts give an application and its agents a common boundary: explicit
inputs and permissions, structured failures, and results that can be checked
again. Implementations stay in their owning projects.

## Try the protocol

Requirements: SBCL with ASDF, GNU Make, and Bash. From this checkout:

```sh
sbcl --script examples/verified-program.lisp run /tmp/rosette-demo
bin/rosette describe /tmp/rosette-demo/composition.json
bin/rosette validate /tmp/rosette-demo/composition.json
sbcl --script examples/verified-program.lisp replay /tmp/rosette-demo
```

The example writes `composition.json`, `receipt.json`, and `verification.json`.
Its program computes `factorial(6) = 720`; its verifier checks the result using
the direct evaluator and an independently lowered kernel VM. Replay reloads
the saved composition and receipt in a fresh process and runs the verifier.

Read [the example's handler and verifier](examples/verified-program.lisp) to
see the integration points. The [protocol walkthrough](docs/quickstart.md)
shows how a well-typed but incorrect result is refused and explains explicit
handler registration for the CLI.

## Connect independent tools

- **Eshkol:** an execution adapter emits the admitted integer program dialect
  and compares direct evaluation, the kernel VM, native JIT, and AOT execution.
- **Moonlab:** circuit data crosses a public ABI boundary; an independent
  quantum VM supplies a state-vector oracle. The included C probe uses
  Moonlab's public symbols.
- **Your implementation:** declare its operation and register its handler and
  verifier. Missing permissions or required evidence produce explicit refusals.

The [integration guide](docs/interop.md) includes actual native examples for
Eshkol and Moonlab, with portable evidence and a second native run to check
reproducibility. Each comparison has an explicit numeric or circuit regime.

Beyond these adapters, the [system reference](docs/systems.md) covers Wire
framing, composition validation, circuit interchange, media projections,
capability-bounded work claims, and the other admitted building blocks.

## Build a component

Start by changing the example's operation and handler. Specify the independent
check your result needs, then include a failing case that the check must reject.

```sh
tools/test-system front-door
make contributor-check
```

[CONTRIBUTING.md](CONTRIBUTING.md) covers adapter and protocol PRs, new examples,
and the route for shared improvements to return upstream and reach related
exports. The public Common Lisp/ASDF source is readable and editable.

## Scope and license

This pre-1.0 runtime uses **SBCL**. Eshkol and Moonlab are independent optional
backends. A verified receipt means its declared checks passed; the strength
of that conclusion depends on those checks and their stated domain.

Apache-2.0. See [architecture](docs/architecture.md),
[export provenance](release/PROVENANCE.md), and [security reporting](SECURITY.md).
`make release-check` verifies a sealed export; use `make contributor-check`
for a working checkout with source changes.
