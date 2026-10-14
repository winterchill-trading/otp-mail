# AGENTS.md: otp-mail/bin

The `otp-mail` command line tool, built from `otp_mail_cli.ml` with `core_unix`'s command module.
Subcommands: `address`, `render`, `dkim-record`, `dkim-verify`, `check-templates`. The usage is in
the README's "Command line" section.

Rules that matter here:

- The CLI never sends email. Keep it that way; sending belongs to `Smtp` in the library.
- Subcommand output is checked by the cram test in `test/cli.t/run.t`. Changing a message or a
  flag changes that file; promote only after reading the diff.
- Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/all` from the repository root.
