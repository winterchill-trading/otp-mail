# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

First release.

### Added

- `Otp`: numeric one-time codes from a CSPRNG, stored as HMAC-SHA256 under a rotatable
  key, with expiry, single use, a purpose per code (sign-in, passcode reset, SCA bound to
  an operation), consecutive-failure limits with doubling lockouts, a resend interval and
  constant-time comparison. Stable `Record`, `Challenge` and `Purpose` types.
- `Otp_store`: the store interface with optimistic concurrency, `Otp_store.Memory` and
  `Otp_store.check`, a contract check for other implementations.
- `Otp_service.Make`: issue and verify over a store, retrying on conflicts.
- `Rng`: `/dev/urandom` and seeded ChaCha20 sources, uniform digits.
- `Address` and `Mailbox`: RFC 5322 addr-spec validation, quoted and encoded display
  names, address lists.
- `Mime`: base64, quoted-printable, RFC 2047 encoded words, header folding, content
  types and an entity parser for nested multiparts.
- `Message` and `Message_id`: rendering to RFC 5322 with MIME and parsing back.
- `Dkim`: rsa-sha256 signing with simple and relaxed canonicalization, PKCS#1 and PKCS#8
  keys, DNS key records, and a verifier for tests.
- `Template`: typed templates per locale with fallback and `Template.Catalogue.check`.
- `Code_email`: a one-time code email in English, German, French, Spanish, Italian and
  Portuguese.
- `Mailer`: the mailer interface, delivery ids, statuses, events, send errors,
  `Event_feed` and `Idempotency`.
- `Simulated_inbox`: an in-memory mailer with queries and per-address outcomes.
- `Smtp`: an SMTP submission client with STARTTLS, implicit TLS, AUTH PLAIN and LOGIN,
  certificate and host name checks, and configuration from the environment.
- `Webhook_sink`: HMAC verification and a deduplicating sink for API mailer webhooks.
- `otp-mail.testing`: `Fake_smtp`, a local SMTP server for tests.
- `otp-mail` command line tool: `address`, `render`, `dkim-record`, `dkim-verify` and
  `check-templates`.
- `Otp.Config.create` accepts a zero `resend_interval`, `lockout_base` and
  `lockout_max`, so tests and demos can switch the waits off (negative spans are still
  refused; `ttl` must still be positive).
- `Otp.verify_challenge` and `Otp.Attempt`: verification against a given challenge id,
  reporting the attempts left on a wrong code; a lockout keeps the challenge, so its id
  still resolves and the code can be retried once the lockout ends. `Otp.attempts_left`
  gives the count for a record.
- `Otp_store.Indexed` (`find_challenge`), implemented by `Otp_store.Memory`, and
  `Otp_store.check_indexed`.
- `Otp_service.Make.verify_challenge`, and `Otp_service.Make_indexed` with
  `verify_by_challenge` for clients that send back only the challenge id and the code.
- `Template.Locale.ru` and `Template.Locale.ky`; `Template.create ?review_pending`,
  `Template.review_pending` and `Template.Catalogue.review_pending` to mark locales whose
  wording awaits a native speaker's review.
- `Code_email`: Russian and Kyrgyz wording (Cyrillic), marked as awaiting review.

### Fixed

- `Template.Catalogue.check` measures the 150-character subject limit in characters, not
  bytes.
