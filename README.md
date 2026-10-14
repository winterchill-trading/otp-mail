# otp-mail

One-time codes and transactional email for OCaml and Async.

- `Otp`: numeric codes from a CSPRNG, stored only as an HMAC under a server key, with an
  expiry, single use, one purpose per code (sign-in, passcode reset, or a payment bound
  to its amount and payee), a failure limit with doubling lockouts, and constant-time
  comparison. Pure: the clock, the random source and the stored record are arguments.
- `Otp_store`, `Otp_service`: a store interface with optimistic concurrency, an
  in-memory store, a contract check for other stores, and a service functor that retries
  on conflicts.
- `Template`, `Code_email`: typed email templates (OCaml functions per locale, no
  template language) with a catalogue check, and a ready-made code email in English,
  German, French, Spanish, Italian, Portuguese, Russian and Kyrgyz (the last two await a
  native speaker's review and are marked so in the catalogue).
- `Address`, `Mailbox`, `Message`, `Mime`: RFC 5322 addresses and display names, MIME
  rendering (quoted-printable, base64, encoded words, header folding,
  `multipart/alternative`) and a parser that reads messages back.
- `Dkim`: DKIM `rsa-sha256` signing with simple or relaxed canonicalization, plus a
  verifier for tests.
- `Mailer`: one sending interface with delivery ids, delivery events and idempotency
  keys; `Simulated_inbox` (in memory, queryable), `Smtp` (STARTTLS or implicit TLS,
  AUTH PLAIN/LOGIN) and `Http_api` (JSON over HTTPS with a bearer token, pluggable request
  and answer shapes) implement it; `Webhook_sink` turns signed webhook deliveries from
  API-based mailers into the same events.
- `otp-mail.testing`: `Fake_smtp`, a local SMTP server for tests.
- `otp-mail` CLI: offline tools (check an address, render the code email, DKIM key
  records and verification, the template check). It never sends email.

## Why separate

Every service that signs people in or confirms payments needs the same few things: a
code that cannot be guessed, replayed, or reused for another purpose; a lockout that
holds under concurrent requests; an email that renders correctly in every client and
language and passes DKIM; and a way to test all of it without sending real mail. These
rules are easy to get subtly wrong (a code stored in plain text, a failure counter reset
by re-sending, a missing `</p>` in one translation, a STARTTLS downgrade) and have
nothing to do with any one service's domain, so they live here, tested once. The library
keeps no state of its own: services implement `Otp_store.S` over their own database.

## Quick start

```ocaml
open! Core
open! Async
open Otp_mail
module Codes = Otp_service.Make (Otp_store.Memory)

let () =
  Command.async ~summary:"demo" (Command.Param.return (fun () ->
    let key = Otp.Key.of_env () |> ok_exn (* OTP_MAIL_OTP_KEY, 32+ bytes hex *) in
    let codes = Codes.create ~key (Otp_store.Memory.create ()) in
    let inbox = Simulated_inbox.create () in
    let email = Address.of_string_exn "zoe@example.org" in
    let%bind issued =
      Codes.issue codes ~subject:"customer-42" ~purpose:Sign_in
      >>| Result.map_error ~f:(fun e -> Error.create_s [%sexp (e : Otp.Issue_error.t)])
      >>| ok_exn
    in
    let message =
      Template.to_message Code_email.template ~locale:Template.Locale.de
        { code = Otp.Code.to_string issued.code
        ; purpose = Sign_in
        ; expires_in = Time_ns.Span.of_min 10.
        ; service_name = "Example Bank"
        }
        ~from:(Mailbox.of_address (Address.of_string_exn "no-reply@bank.example"))
        ~to_:[ Mailbox.of_address email ]
        ()
      |> ok_exn
    in
    let%bind (_ : (Mailer.Delivery_id.t, Mailer.Send_error.t) Result.t) =
      Simulated_inbox.send inbox ~idempotency_key:issued.challenge_id message
    in
    let%map result =
      Codes.verify codes ~subject:"customer-42" ~purpose:Sign_in
        ~code:(Otp.Code.to_string issued.code)
    in
    print_s [%sexp (result : Otp.Verify_result.t)]))
  |> Command_unix.run
```

Complete, runnable programs (their output is checked in CI):
[`examples/sign_in_by_email.ml`](examples/sign_in_by_email.ml) and
[`examples/smtp_with_starttls.ml`](examples/smtp_with_starttls.ml).

## One-time code rules

| Rule | Default | Where |
| --- | --- | --- |
| Digits drawn uniformly from `/dev/urandom` (bytes of 250 and above discarded) | 6 digits (4 to 10) | `Rng.digits`, `Otp.Config.digits` |
| Stored as HMAC-SHA256 over challenge id, purpose and code, under a key with an id (rotation via `previous_keys`) | key from `OTP_MAIL_OTP_KEY` | `Otp.Key` |
| One outstanding code per subject and purpose kind; a new code replaces the old one | | `Otp.issue` |
| A code verifies only for its purpose; SCA codes only for the same binding (amount and payee) | | `Otp.Purpose` |
| Accepted once, until it expires | 10 minutes | `Otp.Config.ttl` |
| Consecutive failures per subject and kind, kept across re-sent codes; the limit starts a lockout | 5 | `Otp.Config.max_attempts` |
| Lockouts double, up to a ceiling; success clears them | 1 min to 1 h (0 allowed, for tests) | `Otp.Config.lockout_*` |
| Minimum time between two codes | 30 s (0 allowed, for tests) | `Otp.Config.resend_interval` |
| Constant-time comparison; codes and keys print as `<redacted>` | | `Otp.verify` |

Rejections carry a reason (`Wrong_code`, `Expired`, `Locked_out`, ...) for audit
records; show the user one message for all of them.

### Verifying by challenge id

`Otp.verify_challenge` (and `Otp_service.Make.verify_challenge`) checks a code against
the challenge id the client was given when the code was issued:

- a stale id (the code was re-sent, used or never existed) is `No_challenge` and counts
  nothing;
- a wrong code answers `Wrong_code { attempts_left }`; re-sending a code does not reset
  the count;
- a lockout keeps the challenge, so the id answers `Locked { until }` during it and the
  same code can be tried after it, until it expires.

With a store that implements `Otp_store.Indexed` (`find_challenge`),
`Otp_service.Make_indexed` also offers `verify_by_challenge ~challenge_id ~code`, for
clients that do not send the subject back.

## Storing records

`Otp_store.S` has two functions, `load` and `save ~expected_version`. A database store
keeps one row per (subject, kind) with the record as `Otp.Record.Stable.V1` (sexp or
bin_prot) and a version column, and saves with
`UPDATE ... WHERE version = expected_version` (or an insert when `expected_version` is
0). `Otp_store.check (module My_store) store` runs the contract scenarios against it.
An `Otp_store.Indexed` store adds a unique index on the outstanding challenge id;
`Otp_store.check_indexed` covers it.

## Sending

| Mailer | Use | Events |
| --- | --- | --- |
| `Simulated_inbox` | tests, demos, local development; query with `messages`, `latest`, `next` | `Sent`, then per-recipient statuses from `~outcome` |
| `Smtp` | a submission relay (port 587 STARTTLS or 465 TLS) | `Sent`; `Failed` for recipients the relay refused |
| `Http_api` | an email API taking JSON over HTTPS | `Sent`; `Failed` on a 4xx refusal; later statuses through `Webhook_sink` |
| your API client + `Webhook_sink.Sink` | an email API | whatever its webhooks report, deduplicated |

`Mailer.send ?idempotency_key` sends once per key (per mailer value) and answers the
same delivery id for a repeat.

### SMTP configuration

`Smtp.Config.of_env ()` reads:

| Variable | Meaning | Default |
| --- | --- | --- |
| `OTP_MAIL_SMTP_HOST` | relay host (and certificate name) | required |
| `OTP_MAIL_SMTP_PORT` | port | 587, 465 for `tls`, 25 for `plain` |
| `OTP_MAIL_SMTP_SECURITY` | `starttls`, `tls` or `plain` | `starttls` |
| `OTP_MAIL_SMTP_USERNAME`, `OTP_MAIL_SMTP_PASSWORD` | AUTH credentials | none |
| `OTP_MAIL_SMTP_HELO` | EHLO name | `localhost` |
| `OTP_MAIL_SMTP_CA_FILE` | trusted roots (PEM) | system bundle |
| `OTP_MAIL_MESSAGE_ID_DOMAIN` | right half of `Message-ID`s | the host |
| `OTP_MAIL_OTP_KEY`, `OTP_MAIL_OTP_KEY_ID` | code HMAC key (hex, 32+ bytes) and its id | required, `k1` |

TLS certificates are verified against the roots and the host name. With `starttls` the
client stops if the server does not offer STARTTLS, and credentials are never sent over
plain TCP unless `allow_plaintext_auth` is set.

### Email API configuration

`Http_api.Config.of_env ()` reads `OTP_MAIL_HTTP_API_URL` (required; `https://`, or
`http://` for loopback only), `OTP_MAIL_HTTP_API_TOKEN` (sent as
`Authorization: Bearer`), `OTP_MAIL_HTTP_API_CA_FILE`,
`OTP_MAIL_HTTP_API_TIMEOUT_SECONDS` and `OTP_MAIL_MESSAGE_ID_DOMAIN`. The body comes from
a `Http_api.Shape.t`: `Shape.fields ()` (from, to, cc, bcc, subject, text, html,
headers), `Shape.raw_mime ()` (the rendered, optionally DKIM-signed message in base64),
or a record of three functions for any other layout. `send ?idempotency_key` forwards
the key as `Idempotency-Key`. 2xx answers are accepted (the id is read from `"id"`), 408,
429 and 5xx are temporary, other 4xx are rejections.

### DKIM

```sh
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out dkim.pem
otp-mail dkim-record -key dkim.pem -selector mail2026 -domain bank.example
```

publish the printed TXT record, then pass
`Dkim.Config.create ~domain:"bank.example" ~selector:"mail2026" key` as `~dkim` to the
mailer.

## Command line

```
otp-mail address 'first.last+tag@example.com'
otp-mail render -locale fr -purpose payment -amount 'EUR 12.50' -payee Acme
otp-mail dkim-record -key dkim.pem -selector mail2026 -domain bank.example
otp-mail dkim-verify -record 'v=DKIM1; k=rsa; p=...' message.eml
otp-mail check-templates
```

## What is verified, and how

| Area | Checked against |
| --- | --- |
| SMTP client | `Fake_smtp` in the tests: plain, STARTTLS, implicit TLS, AUTH PLAIN and LOGIN, wrong password, refused recipients, 4xx and 5xx replies, a refused greeting, a closed port, a server that hides STARTTLS, a certificate for another host, an untrusted chain |
| DKIM | RFC 6376 section 3.4.6 canonicalization example; signatures made here verified by dkimpy 1.1.8 in all four canonicalization pairs, and dkimpy signatures verified here (fixtures) |
| MIME | Quickcheck round trips for quoted-printable, base64, encoded words and header folding; `Message.of_wire (render m) = m` over generated messages; hand-written fixtures with nested multiparts, LF line breaks and ISO-8859-1 words |
| One-time codes | expect scenarios; Quickcheck over random sessions (a code verifies only when fresh, unused and for its purpose; failures never exceed the limit; lockouts hold); chi-squared uniformity of digits; mutation-checked |
| HMAC | RFC 4231 test case 2 |
| Templates | the catalogue check in eight locales, plus a template broken on purpose; Russian minute forms for 1 to 111 |
| Email API client | a loopback HTTP server in the tests: request line and headers, both shapes, chunked and fixed-length answers, 4xx, 429, 5xx, unusable ids, garbage and a server that never answers |

No test sends email anywhere: the SMTP tests talk to a server on 127.0.0.1 started by the
test itself.

## Known gaps

- No connection reuse or pipelining: one SMTP connection per message.
- No MX lookup: `Smtp` submits to one relay; it is not a mail transfer agent.
- No `ed25519-sha256` DKIM (RFC 8463); RSA keys only.
- `Dkim.verify` checks the first signature against a given key; it does not fetch DNS
  records or honour `l=`, `i=` and `x=`.
- No SMTPUTF8: addresses must be ASCII (display names and content may be any UTF-8).
- `Mime.Content_type.parse` does not accept comments or RFC 2231 parameter
  continuations.
- `Http_api` speaks HTTP/1.1 with one connection per message, and follows no redirects
  or proxies.
- The Russian and Kyrgyz code-email wording has not yet been reviewed by native
  speakers (`Template.Catalogue.review_pending` lists it).
- Idempotency keys are remembered in memory per mailer value; a service that sends from
  several processes keeps its own outbox keyed by request.

## Building

```
opam exec -- dune build
opam exec -- dune test
opam exec -- dune build @examples/runtest
```

Needs OpenSSL (async_ssl) and GMP (cryptokit).

## Standards

RFC 5321 (SMTP), RFC 6409 (submission), RFC 3207 (STARTTLS), RFC 8314 (implicit TLS),
RFC 4954 and RFC 4616 (AUTH, PLAIN), RFC 5322 (message format), RFC 2045, 2046 and 2047
(MIME), RFC 6376 and RFC 8301 (DKIM), RFC 2104 and RFC 4231 (HMAC), Commission
Delegated Regulation (EU) 2018/389 articles 4 and 5 (authentication codes and dynamic
linking).

## License

MIT
