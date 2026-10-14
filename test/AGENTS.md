# AGENTS.md: otp-mail/test

Inline tests for the library (ppx_expect with `-source-tree-root .`), Quickcheck properties
(`test_otp_properties.ml`), and a cram test of the CLI in `cli.t/`. Run from the repository root:
`opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/runtest`.

- `fixtures/` holds `.eml` and DKIM key files; `certs/` holds a test CA and localhost certificates.
  Both are test data and are declared as `glob_files` dependencies in `dune`. Keep them binary
  (`-text` in `.gitattributes`).
- The CA key was deleted after signing, so the committed CA cannot sign anything new. Regenerating
  certificates means new fixtures and a new CA.
- Promote expect output only after reading the diff. Expect blocks must not contain `|}`.
- Mail in tests goes to `Simulated_inbox` or `testing/`'s `Fake_smtp`, never to a real host.
