# Contributing

Clone [rosette-wire](https://github.com/RichardHoekstra/rosette-wire), make a focused branch, and open a pull request.
You need only this public checkout, SBCL, GNU Make, and Bash. No private
repository, model service, or maintainer tooling is required to develop a change.

## Develop and check

```sh
tools/test-system front-door
make contributor-check
```

The focused command loads the checkout's registry and tests one ASDF system,
then lists the test suites it ran. Name the primary system (`front-door`, not
`front-door/tests`) to run all of its suites; the tool names any it skipped.

Adapter checks against a real external toolchain are opt-in. For Eshkol, set
`ROSETTE_ESHKOL_BIN=/path/to/eshkol-run`, or `ROSETTE_ESHKOL_CONTAINER=<name>`
for a running container with `/eshkol/build/eshkol-run` and the host's
temporary directory mounted at the same path. The isolated worker needs the
Linux `prlimit` utility; without it these checks cannot run under isolation.
`contributor-check` runs all public systems, CLI and example checks, documentation
links, and hygiene. PR CI runs this target. Add a negative control when changing
identity, capability, verifier, or refusal behavior.

The release identity describes the last emitted snapshot, not your edited branch.
Leave `release/EXPORT-ID` and `release/EXPORT-MAP.sexp` unchanged. The maintainer
regenerates these records when accepting the change. `make release-check` is the
separate sealed-snapshot gate and intentionally rejects edits to a sealed tree.

## Existing mapped text files

For source edits admitted by the distribution lens, attach a proposal bundle:

```sh
tools/propose-upstream /tmp/upstream-change.sexp
```

Choose a fresh output path outside the checkout. This records the original
identities and proposed replacements. The maintainer lifts the bundle into the
canonical source, reviews it, runs verifiers, and regenerates the public tree.

## New files, deletions, build definitions, and generated documents

These are welcome through ordinary PRs. The current lens deliberately refuses
them, so **a bundle is optional for these PRs**. Include the normal Git diff,
the base commit and export identity, why each file is added or removed, and the
test command covering it. Put a new source file in the relevant system and
declare it in its ASDF components; tests for an existing system run in PR CI.
Propose a new public system in an issue first because it changes the distribution
closure. Do not edit the frozen release manifests to bypass closure checks.

The maintainer then:

1. Maps each proposed file to its canonical owner, including the export generator
   for generated documents and build files.
2. Applies the reviewed diff there, preserving the contributor's author identity
   and PR reference. Reviews any dependency, public API, or export-grant change.
3. Runs the affected verifiers and regenerates a fresh public candidate.
4. Compares the regenerated change against the PR, explains any differences,
   runs the release gate, and links the landing commit and release from the PR.

The public PR is the review record throughout. A lens refusal does not reject
the contribution. Maintainers handle upstream integration; contributors do not
need access to the canonical repository. Regeneration must not silently erase
accepted changes, attribution, or unrelated public changes.

Contributions are submitted under Apache-2.0, section 5 of [LICENSE](LICENSE).
Use [SECURITY.md](SECURITY.md) for private reports.
