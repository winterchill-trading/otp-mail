# Notes on the protocols

What was learned building otp-mail, for whoever extends it.

## SMTP (RFC 5321, 6409, 3207, 8314, 4954)

- Replies are `NNN` then `-` (more lines follow) or a space (last line). The code on every
  line is the same; read until the line whose fourth character is a space.
- After `STARTTLS` and its `220`, the client must forget everything it learned and send
  `EHLO` again: the server may advertise different extensions (typically `AUTH` only
  appears once encrypted). The capability list from before TLS cannot be trusted, which
  is also why a client set to require STARTTLS must stop, not fall back, when the
  capability is missing (a man in the middle can strip it).
- `AUTH PLAIN` takes `base64("\0" ^ user ^ "\0" ^ password)` on the same line.
  `AUTH LOGIN` is two `334` round trips (the prompts are base64 of `Username:` and
  `Password:`). Never log these lines.
- `RCPT TO` is answered per recipient; a relay may refuse some and accept others, and
  `DATA` is still allowed if at least one was accepted. A refusal of every recipient
  ends the transaction.
- Dot-stuffing: every line that starts with `.` gets another `.`; the data ends with
  `CRLF . CRLF`, so a message that does not end with CRLF needs one added first.
- Implicit TLS on 465 (RFC 8314) is preferred by the RFC over STARTTLS on 587 for
  submission; both are supported.
- `4xx` is temporary (retry later), `5xx` permanent. `535` is a failed AUTH.

## TLS from Async

- `Async_ssl.Tls.wrap_client_connection` exists for exactly the STARTTLS case: it takes
  the plain `Reader.t`/`Writer.t` and gives encrypted ones to a callback, and closes the
  TLS session when the callback returns. The server side has
  `wrap_server_connection`.
- OpenSSL checks the chain, but the host name is only checked if the
  `verify_callback` calls `Ssl.Connection.check_peer_certificate_host`; `remote_hostname`
  alone is SNI. A mismatch then fails with `Connection verification failed.
  (connection_verification_error "hostname did not match")`, an unknown chain with
  `certificate verify failed`.
- Give an explicit `ca_file`: the client looks for `SSL_CERT_FILE`, the Debian bundle,
  `/etc/ssl/cert.pem`, then Homebrew's OpenSSL bundle.

## MIME (RFC 2045, 2046, 2047, 5322)

- Quoted-printable lines are at most 76 characters including the `=` of a soft break,
  so wrap at 75 unless the token is the last on the line. Never split an `=XX` escape.
  Encode trailing spaces and tabs (`=20`, `=09`): relays may strip them.
- The boundary trick: `=_` cannot occur in quoted-printable output (a literal `=` is
  always `=3D`) or in base64, so a boundary starting with `=_` is safe for encoded parts.
  For 7bit parts the boundary is derived from a hash of the content and checked.
- Encoded words are at most 75 characters, must not split a UTF-8 sequence, and the
  whitespace between two adjacent encoded words is not part of the text (RFC 2047
  section 6.2), so long values are split into several words separated by spaces, which
  is also where the header folder can break the line.
- Header folding inserts CRLF before existing whitespace; unfolding just removes the
  CRLF. A line made only of whitespace is not allowed.
- RFC 5322 dates: the weekday and month names are English three-letter forms, the zone
  is `+0000` when UTC.

## DKIM (RFC 6376, 8301)

- Relaxed header canonicalization: lower-case the name, unfold, squeeze whitespace runs
  to one space, strip whitespace around the value and around the colon. Simple:
  byte for byte.
- Relaxed body: squeeze whitespace, strip trailing whitespace on each line, remove
  trailing empty lines, and an empty body is empty. Simple: remove trailing empty lines
  but an empty body is one CRLF.
- The header hash covers the fields named in `h=` (each name picks the next unused
  instance from the bottom) and then the `DKIM-Signature` field itself with the `b=`
  value removed and no final CRLF. Fold the signature field before hashing, because with
  simple canonicalization the folding is signed.
- Listing `from` once more than it occurs ("oversigning") makes a `From` added later
  break the signature instead of being ignored.
- The signature is RSASSA-PKCS1-v1_5: `00 01 FF.. 00 || DigestInfo(SHA-256) || hash`,
  then the private-key operation. Cryptokit's `RSA.sign` is the raw operation, so the
  padding is built by hand; its output is left-padded to the modulus length.
- DNS TXT strings are at most 255 bytes; a 2048-bit key record is longer, so zone files
  carry it as several quoted strings that resolvers join.
- Interoperability was checked both ways with dkimpy 1.1.8.

## One-time codes and PSD2 SCA (EU 2018/389)

- Article 4: a code is accepted once, consecutive failures before a block "shall not
  exceed five", and a failure must not reveal which element was wrong.
- Article 5 (dynamic linking): the payer sees amount and payee, and the code is specific
  to them; any change invalidates the code. Here the caller passes a binding (a digest
  over amount, currency and payee) and it is part of the HMAC input.
- The RTS sets no lifetime for an unanswered code; 10 minutes is a common choice for
  email delivery delays. Five minutes of inactivity is the rule for sessions, not codes.

## Environment quirks

- Cryptokit 1.16's `Random.system_rng` raises on every Unix; read `/dev/urandom` with
  `Random.device_rng`.
- A git checkout with `core.autocrlf=input` converts CRLF fixtures to LF on commit;
  `.gitattributes` marks `test/fixtures/**` as `-text`.
