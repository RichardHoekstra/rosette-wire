.PHONY: contributor-check docs-check test systems cli-smoke examples hygiene verify-export-id upstream-bundle publication-check clean-room reproducible-release release-check dist

contributor-check: test

docs-check:
	@sbcl --script tools/docs-check

test: systems cli-smoke examples hygiene docs-check

systems:
	@sbcl --script tools/verify-systems

cli-smoke:
	@sbcl --script tools/cli-smoke

examples:
	@tools/examples-smoke

hygiene:
	@tools/hygiene

verify-export-id:
	@sbcl --script tools/verify-export-id

upstream-bundle:
	@test -n "$(OUTPUT)" || { echo 'usage: make upstream-bundle OUTPUT=/absolute/path/bundle.sexp' >&2; exit 2; }
	@tools/propose-upstream "$(OUTPUT)"

publication-check:
	@tools/publication-check

clean-room:
	@tools/clean-room

reproducible-release:
	@tools/reproducible-release

release-check: test verify-export-id reproducible-release clean-room publication-check

dist:
	@tools/build-release
