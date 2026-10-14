(** One-time codes and transactional email.

    Codes are issued and checked by {!Otp} (pure) over a {!Otp_store.S} through
    {!Otp_service}; emails are typed {!Template}s rendered to {!Message}s and handed to a
    {!Mailer}: {!Simulated_inbox} in tests and demos, {!Smtp} for a real relay.

    {[
      open! Core
      open! Async
      open Otp_mail
      module Codes = Otp_service.Make (Otp_store.Memory)

      let send_sign_in_code codes mailer ~customer_id ~email ~locale =
        match%bind Codes.issue codes ~subject:customer_id ~purpose:Sign_in with
        | Error e -> return (Error (Error.create_s [%sexp (e : Otp.Issue_error.t)]))
        | Ok issued ->
          let params =
            { Code_email.code = Otp.Code.to_string issued.code
            ; purpose = Sign_in
            ; expires_in = Time_ns.Span.of_min 10.
            ; service_name = "Example Bank"
            }
          in
          (match
             Template.to_message
               Code_email.template
               ~locale
               params
               ~from:(Mailbox.of_address (Address.of_string_exn "no-reply@bank.example"))
               ~to_:[ Mailbox.of_address email ]
               ()
           with
           | Error e -> return (Error e)
           | Ok message ->
             (match%map
                Mailer.send mailer ~idempotency_key:issued.challenge_id message
              with
              | Ok _ -> Ok ()
              | Error e -> Error (Error.create_s [%sexp (e : Mailer.Send_error.t)])))
      ;;

      let check_code codes ~customer_id ~code =
        match%map Codes.verify codes ~subject:customer_id ~purpose:Sign_in ~code with
        | Verified -> `Signed_in
        | Rejected _ -> `Try_again (* one message for every reason *)
      ;;
    ]}

    A complete program is [examples/sign_in_by_email.ml]; SMTP with STARTTLS is
    [examples/smtp_with_starttls.ml].

    {1 Modules}

    One-time codes:
    - {!Otp}: codes, purposes, records and the pure issue and verify rules.
    - {!Otp_store}: where records live; an in-memory store and a contract check.
    - {!Otp_service}: issue and verify over a store, with conflict retries.
    - {!Rng}: the CSPRNG, and seeded generators for tests.

    Email:
    - {!Address}, {!Mailbox}: RFC 5322 addresses and display names.
    - {!Message}, {!Message_id}: messages rendered to MIME and parsed back.
    - {!Mime}: transfer encodings, encoded words, header folding and an entity parser.
    - {!Dkim}: DKIM rsa-sha256 signing (and verification for tests).
    - {!Template}, {!Code_email}: typed templates per locale and the code email.
    - {!Mailer}: the sending interface, delivery events and idempotency keys.
    - {!Simulated_inbox}, {!Smtp}: mailers.
    - {!Webhook_sink}: delivery events from API-based mailers.

    The [otp-mail.testing] library has [Fake_smtp], a local SMTP server for tests. *)

module Mime = Mime
module Address = Address
module Mailbox = Mailbox
module Message_id = Message_id
module Message = Message
module Dkim = Dkim
module Rng = Rng
module Otp = Otp
module Otp_store = Otp_store
module Otp_service = Otp_service
module Template = Template
module Code_email = Code_email
module Mailer = Mailer
module Simulated_inbox = Simulated_inbox
module Smtp = Smtp
module Webhook_sink = Webhook_sink
