# AGENTS.md: otp-mail

## Purpose

otp-mail issues and checks one-time codes (stored only as an HMAC, single use, one purpose per
code, lockouts) and builds, signs and sends transactional email over SMTP, with DKIM and typed,
localised templates. It keeps no state of its own and never sends mail from its CLI, and its
tests use an in-memory inbox or a local fake SMTP server.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `otp-mail` library (module `Otp_mail`): `Otp`, `Otp_store`, `Otp_service`, `Template`, `Code_email`, `Address`, `Mailbox`, `Message`, `Mime`, `Dkim`, `Mailer`, `Simulated_inbox`, `Smtp`, `Http_api`, `Webhook_sink` |
| `bin/` | `otp-mail` CLI: offline tools (address check, render, DKIM record and verify, template check) |
| `testing/` | `otp-mail.testing` library: `Fake_smtp`, a local SMTP server for tests |
| `test/` | Inline expect tests, properties, the cram test `cli.t/`, and `fixtures/` and `certs/` (test data only). `test_http_api.ml` runs its own loopback HTTP server; `test_otp_challenge.ml` and `test_otp_service_indexed.ml` cover verification by challenge id |
| `examples/` | Two programs whose output is diffed against `.expected` files in `runtest`, and `doc_example.ml`, a library kept in step with `lib/otp_mail.mli` |
| `docs/` | `api-notes.md`: protocol notes (SMTP, TLS, MIME, DKIM, SCA) and environment quirks |
| `.github/` | `workflows/ci.yml` |

`README.md` has the module overview, the CLI usage, what is verified and the known gaps.
`CHANGES.md` keeps the `Unreleased` notes.

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/all
opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/runtest
```

`runtest` includes the expect tests, the cram test of the CLI and the example diffs. For parallel
work, use `DUNE_BUILD_DIR=_build-otp-mail-<task>`.

Standalone (outside the workspace): `opam install . --deps-only --with-test`, then
`dune build && dune test`. Building needs OpenSSL (async_ssl) and GMP (cryptokit).

## Conventions

- Format only this repository: `dune build --root .. @otp-mail/fmt --auto-promote`, after the
  tests pass. Never run `dune fmt` at the workspace root.
- Every module has an `.mli`. Use `open! Core`, labelled arguments and `ppx_jane`.
- Pure where possible: the clock, the random source and the stored record are arguments.
  `Rng` is the only module that reads the kernel random source.
- Stable bin_prot types (in `Address`, `Mailbox`, `Message`, `Otp`, `Template` and others) have
  digests pinned in the tests, for example `test/test_address.ml`. A changed digest needs a new
  `Stable.V2`.
- Tests that depend on TLS failure text match substrings, never full messages.
- odoc on every public value. User-visible changes get a line under `Unreleased` in `CHANGES.md`.

## Dependencies and dependents

- No sibling repository is built against. Dependencies are public packages: core, async,
  async_ssl, cryptokit (1.16 or later), yojson (2.2 up to 4.0), ppx_jane; core_unix for
  the CLI and tests.
- Dependents in the workspace: none at present.
- CI pins nothing: `ox-setup` runs without a `pins:` block.

## CI

`.github/workflows/ci.yml` runs on every push and pull request: the `ox-setup` composite action
from `winterchill-trading/infra`, then `dune build`, `dune test` and `dune build @examples/runtest`,
then the `obj-magic` reusable workflow from the same repository. To check a run, use
`gh run list --limit 1`, and at most one `gh run watch <id> --exit-status`. Do not poll in a loop.

## Pitfalls

- `Random.system_rng` raises on Unix. Read the kernel source with `Random.device_rng "/dev/urandom"`.
  Tests use `Random.pseudo_rng` with a seed of at least 16 bytes.
- Cryptokit's RSA signing is the raw private-key operation. Build the PKCS#1 v1.5 padding yourself.
- This machine has `core.autocrlf=input`. Keep `test/fixtures/** -text` in `.gitattributes` or CRLF
  fixtures are committed as LF; check with `git ls-files --eol`.
- After STARTTLS the client sends EHLO again and must not fall back to plain text when TLS is required.
- An expect block whose output contains `|}` ends the quoted string early; reorder the test input.
- Failing `runtest` then `@otp-mail/fmt --auto-promote` leaves nothing to promote. Promote first.
- `Message_id.to_string` already includes the angle brackets; do not add another pair.
- Tests that set environment variables use `Core_unix.putenv` with
  `[@alert "-unsafe_multidomain"]`; pick variable names no other test reads.
- `Template.Catalogue.check` counts subject length in characters (UTF-8 aware), not
  bytes.
- A `?time_source` argument with a default of `Time_source.wall_clock ()` closes the parameter's
  type. Take it as an option and match inside.
- `Otp_store.Indexed` is both a module type and, in the module namespace, the adapter
  functor `Otp_store.Indexed (Store)`. The adapter's index is per process; tests that
  run two services over one store need different `Rng` seeds, or both issue the same
  challenge id.

## Where to read more

- `README.md` (rules for codes, the sending model, DKIM, the CLI, what is verified, known gaps).
- `docs/api-notes.md` for the RFC notes behind SMTP, TLS, MIME and DKIM.
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide), `../oxcaml-claude-skills/skills/banking-standards/`
  for the PSD2 SCA rules, and the section "otp-mail" of `../oxcaml-claude-skills/GOTCHAS.md`.

## Never do

- Install an opam package that changes the compiler, `core`, `base`, `async` or `ppx_jane` on the
  shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Send mail to real addresses or relays from tests or examples; use `Simulated_inbox` or `Fake_smtp`.
- Reuse the keys under `test/certs/` or `test/cli.t/` outside tests.
- Publish, tag or release.
