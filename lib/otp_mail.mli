(** One-time codes and transactional email.

    {1 Modules}

    - {!Mime}: transfer encodings, encoded words, header folding and an entity parser. *)

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
