# AGENTS.md: otp-mail/examples

Two programs built with the library, not installed. Their output is compared with the
`.expected` files by rules in `dune` (`@otp-mail/runtest`).

- `sign_in_by_email.ml`: a sign-in code issued, rendered, sent into a simulated inbox, read back
  and verified, with nothing leaving the process. Its seed and synthetic clock keep the output stable.
- `smtp_with_starttls.ml`: sends one message through `Fake_smtp` with STARTTLS, using the test
  certificates in `../test/certs`.
- `doc_example.ml` (a library): the example from the top of `lib/otp_mail.mli`, compiled so it
  cannot drift. Change the two together.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @otp-mail/all`; CI runs
`dune build @examples/runtest` on its own.

When an example's output changes on purpose, update the `.expected` file in the same commit and
say why in the message.
