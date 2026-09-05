# Eshkol and Moonlab interoperability

[Eshkol](https://github.com/tsotchke/eshkol) is an independent execution backend.
The public `front-door/eshkol` subsystem already gates an admitted integer
program across the direct evaluator, kernel VM, cache-disabled LLVM JIT, and
separately compiled and executed AOT artifact.

Build or install Eshkol using its own instructions, then explicitly opt in:

```sh
ROSETTE_ESHKOL_BIN=/path/to/eshkol-run sbcl --script examples/verified-program.lisp eshkol
ROSETTE_ESHKOL_BIN=/path/to/eshkol-run tools/test-system front-door/eshkol
```

Replace `/path/to/eshkol-run` with your executable. Nothing is downloaded or
built implicitly. The example's `eshkol` command fails if the toolchain is
missing; it cannot report a CPU-only run as four-way agreement. The regular
CPU tests report unconfigured external integrations as skipped. Test fixtures
that emulate process replies test adapter behavior, not real compiler parity.

The accepted dialect includes exact integer arithmetic, conditionals, lexical
binding, and admitted recursion. Every value and intermediate must fit the
tagged-i32 kernel regime `-2^28 <= value < 2^28`. General higher-order programs,
rational/float AD, and arbitrary ProgramIR dialects are outside this gate.

The external gate returns an execution observation, not a self-contained
toolchain-attested certificate. Include compiler revision, commands, stdout,
the exported version, and export identity when reporting a real compiler run.
Do not generalize a finite agreement result to all programs.

To contribute a new floor or numeric regime, open a public issue describing the
input contract, preserved semantics, independent oracle, resource bounds, and a
negative control. Keep each implementation in its owning project; exchange
explicit inputs and evidence through the public boundary.

## Reproduce native execution and replay

Run this integer least-squares example to compare the direct evaluator, kernel
VM, Eshkol JIT, and Eshkol AOT. The samples follow `y=2*x+1`, for `x=0..4`;
the candidate `y=x+1` has a sum of squared residuals of **30**.

```sh
export ROSETTE_ESHKOL_BIN=/path/to/eshkol-run
export ROSETTE_ESHKOL_ID=eshkol-your-revision
sbcl --script examples/external-backends.lisp eshkol /tmp/rosette-native
```

Use a path-free version or revision for the implementation ID. This saves a
portable JSON bundle, decodes and verifies it, reruns the actual external
backend and checks exact bundle replay, then rejects a corrupted transfer.
It exits nonzero on missing execution, disagreement, or a failed control.

## Moonlab

[Moonlab](https://github.com/tsotchke/moonlab) is the independent quantum
laboratory. `quantum-ir` carries circuit data; `quantum-vm` supplies the local
semantic oracle; `wire-diagnostics` implements bounded process/ABI campaigns
and replayable diagnostic bundles. Both exports include these systems.

```sh
tools/test-system quantum-vm
tools/test-system wire-diagnostics
```

Build the included public ABI consumer (a C11 compiler and libdl are needed):

```sh
cc -std=c11 -O2 src/compiler/wire/wire-diagnostics/tools/moonlab-abi-probe.c -ldl -lm -o /tmp/moonlab-abi-probe
export ROSETTE_MOONLAB_PROBE=/tmp/moonlab-abi-probe
export ROSETTE_MOONLAB_LIB=/path/to/libquantumsim.so
export ROSETTE_MOONLAB_ID=moonlab-your-revision
sbcl --script examples/external-backends.lisp moonlab /tmp/rosette-native
```

The example executes a two-qubit circuit with `H`, `RY`, `CNOT`, and `RZ` in
Moonlab and the independent quantum VM. It compares complete state vectors up
to global phase, checks ownership and invalid-target behavior, saves a portable
bundle, performs a second native run, and rejects a corrupted transfer.

The default system tests do not claim that a native Moonlab library ran. A real
campaign explicitly supplies an ABI probe command, a Moonlab shared-library
path, and an implementation identity through `run-moonlab-campaign` or
`run-moonlab-surface-campaign`. The admitted surface campaign covers state,
measurement, channels, QGT/gradient, and ownership/error behavior in bounded
regimes. Missing external execution is a distinct outcome.

Use the `wire-diagnostics` entry in [the system reference](systems.md) for the
exported adapter source, probe fixture, public signatures, and replay tests.
Report ABI revision and exercised capabilities with results; an optional GPU
path cannot be counted as exercised merely because the CPU oracle passed.
