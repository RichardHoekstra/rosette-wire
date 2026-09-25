# Changelog

All notable changes follow semantic versioning.

## [Unreleased]

- Extended the `front-door/eshkol` gate with a second, independently admitted
  numeric dialect covering exact rationals, IEEE doubles under an explicit
  ulp regime, and forward-mode AD through Eshkol's `derivative`/`gradient`
  builtins. `:int` promotes freely into `:rational` or `:float`; the two
  never implicitly mix. See docs/interop.md.
- `:float` readouts from Eshkol cross the process boundary as an exact
  `(:dyadic m e)` encoding instead of a decimal display, so the ulp regime
  compares the double Eshkol computed rather than its rounded print.
- `tools/test-system` lists the test suites it ran and names any suites of the
  same primary system it did not run.

## [0.1.0-rc.3] - 2026-09-01

- First publication candidate compiled through the audited Apache-2.0 membrane.
- Added standalone tests, CLI examples, deterministic source archives, and
  clean-room publication gates.
