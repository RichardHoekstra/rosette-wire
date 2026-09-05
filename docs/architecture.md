# Architecture

Rosette is published as a compiled source distribution. The readable
Common Lisp/ASDF tree is the public execution floor, not a manually maintained
fork. Its distribution definition fixes semantic roots, dependency closure,
namespace, license grants, forbidden families, entrypoints, and hygiene rules.

Four identities are deliberately separate:

- `release/SOURCE-CUT` identifies the audited semantic cut;
- `release/DISTRIBUTION-ID` identifies the compiled dependency plan;
- `release/EXPORT-MAP.sexp` authenticates file provenance and the partial lens; and
- `release/EXPORT-ID` identifies this standalone renamed tree.

`make verify-export-id` recomputes the tree identity independently. A backend,
client, or model is not authoritative merely because it produced an artifact.
The deterministic Rosette verifier remains the validation boundary.

The distribution lens gives the generated tree a checked return path. Direct
text views admit bounded edits when the source, map, and export identities still
match; filtered, generated, binary, added, and deleted artifacts produce typed
obstructions. `tools/propose-upstream /absolute/path/to/bundle.sexp` creates an
inert proposal and never executes contributed code. Maintainers lift that bundle
to a fresh source overlay, review it, run ordinary gates, and regenerate the
distribution. Neither side applies, commits, merges, pushes, or publishes by
implication.
