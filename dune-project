(lang dune 3.17)

(name otp-mail)

(generate_opam_files true)

(license MIT)

(maintainers "hello@winterchill.xyz")

(authors "WinterChill AI")

(source
 (github winterchill-trading/otp-mail))

(homepage "https://github.com/winterchill-trading/otp-mail")

(bug_reports "https://github.com/winterchill-trading/otp-mail/issues")

(documentation "https://github.com/winterchill-trading/otp-mail#readme")

(package
 (name otp-mail)
 (synopsis "One-time codes and transactional email for Async")
 (description
  "otp-mail issues and checks one-time codes (CSPRNG digits, hashed at rest, expiry, attempt limits with lockout backoff, one purpose per code) and sends transactional email: typed templates per locale with a catalogue check, RFC 5322 addresses, MIME (quoted-printable, base64, multipart/alternative, header folding), DKIM rsa-sha256 signing, an in-memory inbox for tests and demos, an SMTP client with STARTTLS and implicit TLS, and a JSON-over-HTTPS client for email APIs.")
 (tags
  (otp email smtp mime dkim async))
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (core
   (and
    (>= v0.17.2)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.17.1)
    (< v0.19~)))
  (async
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (async_ssl
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (cryptokit
   (and
    (>= 1.16)
    (< 2.0)))
  (yojson
   (and
    (>= 2.2)
    (< 4.0)))
  (odoc :with-doc)))
