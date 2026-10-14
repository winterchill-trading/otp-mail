(* The example from the top of lib/otp_mail.mli. *)

open! Core
open! Async
open Otp_mail
module Codes : module type of Otp_service.Make (Otp_store.Memory)

val send_sign_in_code
  :  Codes.t
  -> Mailer.t
  -> customer_id:string
  -> email:Address.t
  -> locale:Template.Locale.t
  -> unit Or_error.t Deferred.t

val check_code
  :  Codes.t
  -> customer_id:string
  -> code:string
  -> [ `Signed_in | `Try_again ] Deferred.t
