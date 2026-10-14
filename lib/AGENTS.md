# AGENTS.md: otp-mail/lib

The `otp-mail` library. Every module has an `.mli`; odoc on every public value.

Build from the repository root: `opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/all`.

Rules that matter here:

- `Otp` is pure: the clock, the random source and the stored record are arguments. Codes are
  stored as an HMAC, never in clear, and compared in constant time.
- `Rng` is the only module that reads the kernel random source (`/dev/urandom`, see the root file).
- `Smtp` is the only module here that opens network connections. `Simulated_inbox` is the
  in-memory mailer that tests and examples use.
- Templates are OCaml functions per locale, not a template language. A new locale must pass
  `otp-mail check-templates`.
- A new module needs: `.ml` and `.mli`, expect tests in `test/`, and a row in the README overview.
