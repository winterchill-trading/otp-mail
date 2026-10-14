Addresses are checked and split.

  $ otp-mail address 'first.last+tag@Example.COM'
  local part: first.last+tag
  domain:     example.com
  $ otp-mail address 'two..dots@example.com'
  ("invalid email address" "bad local part" two..dots@example.com)
  [1]

The built-in templates pass their own check.

  $ otp-mail check-templates
  no problems

Rendering is reproducible with a seed and a date, and signing with a key gives a
message that verifies against the key's DNS record.

  $ otp-mail render -locale fr -purpose payment -amount 'EUR 12.50' -payee 'Acme' -code 123456 -seed cli-test-seed-0001 -date '2026-10-14 09:30:00Z' > plain.eml
  $ grep -c 123456 plain.eml
  2
  $ head -5 plain.eml | tr -d '\r'
  Date: Wed, 14 Oct 2026 09:30:00 +0000
  From: Example Bank <no-reply@bank.example>
  To: customer@example.org
  Subject:
   =?UTF-8?B?Q29uZmlybWV6IHZvdHJlIHBhaWVtZW50IGRlIEVVUiAxMi41MCDDoCBBY21l?=
  $ otp-mail render -code 123456 -seed cli-test-seed-0001 -date '2026-10-14 09:30:00Z' -dkim-key key.pem -dkim-selector s1 > signed.eml
  $ head -1 signed.eml | tr -d '\r'
  DKIM-Signature: v=1;
  $ otp-mail dkim-record -key key.pem -selector s1 -domain bank.example | cut -c 1-60
  s1._domainkey.bank.example TXT "v=DKIM1; k=rsa; p=MIIBIjANBg
  $ RECORD=$(otp-mail dkim-record -key key.pem -selector s1 -domain bank.example | sed 's/^[^"]*"//; s/" "//g; s/"$//')
  $ otp-mail dkim-verify -record "$RECORD" signed.eml
  signature verifies
  $ sed 's/123456/654321/' signed.eml > tampered.eml
  $ otp-mail dkim-verify -record "$RECORD" tampered.eml
  "body hash does not match"
  [1]
