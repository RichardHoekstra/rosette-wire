# Rosette quick start

Requirements: SBCL with ASDF, GNU Make, and Bash. Run from the checkout root.

## Execute, verify, and replay a real program

```sh
sbcl --script examples/verified-program.lisp run /tmp/rosette-demo
sbcl --script examples/verified-program.lisp replay /tmp/rosette-demo
```

The example computes `factorial(6) = 720`. Its Component executes the existing
front-door program, whose direct evaluator and independently lowered kernel VM
must agree. A required verifier reruns that computation and checks the result
in the execution receipt. The first command writes `composition.json`,
`receipt.json`, and `verification.json`; the second reloads the first two in a
fresh process and verifies them again. Use an output directory outside the checkout.

Inspect these JSON files to see the Component's typed output, implementation
identity, one-step execution budget, required evidence, result, and verdict.
The source is [verified-program.lisp](../examples/verified-program.lisp): handler
and verifier registration are explicit, so this is also an adapter example.

## See a plausible wrong answer refused

```sh
sbcl --script examples/verified-program.lisp mutate /tmp/rosette-mutant
```

This intentionally changes the handler's answer to `721`. Execution still
satisfies the integer Port type, but independent verification fails. The command
prints a `fail` verdict and exits **1**, which is the expected result. The honest
run and replay exit **0**. The verifier is what detects the incorrect answer.

This bounded integer example establishes execution agreement, not symbolic
algebra, rational AD, or an unrestricted theorem. See the
[Eshkol interoperability guide](interop.md) to add real JIT/AOT execution.

## Inspect the public protocol

```sh
bin/rosette describe /tmp/rosette-demo/composition.json
bin/rosette validate /tmp/rosette-demo/composition.json
```

The CLI also provides `connect`, `run`, `verify`, and `certify`. A fresh CLI
runner does not automatically register the example's trusted handler/verifier;
use the example's replay command for this receipt. `verify` requires an explicit
`--graph` document; content-address lookup is not yet installed. Missing required
handlers or verifiers are unavailable, never a pass.

## Develop an adapter

```sh
tools/test-system front-door
make contributor-check
```

Start with the example, change its Component and handler, and declare the
independent evidence your operation needs. The [system reference](systems.md)
links the exact exported implementation and tests. Read
[CONTRIBUTING.md](../CONTRIBUTING.md) for the public PR workflow.
