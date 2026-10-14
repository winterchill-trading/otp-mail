# AGENTS.md: otp-mail/testing

The `otp-mail.testing` library: `Fake_smtp`, a small SMTP server for tests. It listens on
127.0.0.1 on an ephemeral port, speaks enough ESMTP for `Smtp` (STARTTLS, implicit TLS, AUTH),
keeps every message it accepts, and can be told to refuse a recipient, hide STARTTLS or demand AUTH.

Build from the repository root: `opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/all`.

Rules that matter here:

- It never relays. Keep it a test double.
- It depends on `otp-mail` and must not be depended on by the library itself, or the test
  dependency leaks into the installed package.
