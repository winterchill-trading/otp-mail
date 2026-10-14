(* The example from the top of lib/otp_mail.mli, compiled so that it cannot drift from the
   library. Keep the two in step. *)

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
       (match%map Mailer.send mailer ~idempotency_key:issued.challenge_id message with
        | Ok _ -> Ok ()
        | Error e -> Error (Error.create_s [%sexp (e : Mailer.Send_error.t)])))
;;

let check_code codes ~customer_id ~code =
  match%map Codes.verify codes ~subject:customer_id ~purpose:Sign_in ~code with
  | Verified -> `Signed_in
  | Rejected _ -> `Try_again (* one message for every reason *)
;;
