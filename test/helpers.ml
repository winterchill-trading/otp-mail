open! Core
open Otp_mail

let address = Address.of_string_exn
let mailbox ?name a = Mailbox.create ?display_name:name (address a)

(* Shows CRLF explicitly so expect output pins the exact line breaks. *)
let show_wire s =
  print_string (String.substr_replace_all s ~pattern:"\r\n" ~with_:"$\n");
  if not (String.is_suffix s ~suffix:"\n") then print_endline "<no final line break>"
;;

let fixed_date = Time_ns.of_string_with_utc_offset "2026-10-14 09:30:00Z"

let fixed_message_id =
  Message_id.create ~local:"0123456789abcdef" ~domain:"bank.example" |> Or_error.ok_exn
;;
