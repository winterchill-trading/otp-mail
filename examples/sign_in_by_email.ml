(* Sign-in by emailed code, end to end, with nothing leaving the process: a code is
   issued, rendered with the built-in template, "sent" into a simulated inbox, read back
   out of the received message and verified. The seed and the synthetic clock make the
   output the same on every run; a service would use [Rng.system ()] and the wall clock. *)

open! Core
open! Async
open Otp_mail
module Codes = Otp_service.Make (Otp_store.Memory)

let customer = Address.of_string_exn "zoe@example.org"

let sender =
  Mailbox.create
    ~display_name:"Example Bank"
    (Address.of_string_exn "no-reply@bank.example")
;;

let send_code codes inbox ~locale =
  match%bind Codes.issue codes ~subject:"customer-42" ~purpose:Sign_in with
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
         ~from:sender
         ~to_:[ Mailbox.of_address customer ]
         ()
     with
     | Error e -> return (Error e)
     | Ok message ->
       (match%map
          Simulated_inbox.send inbox ~idempotency_key:issued.challenge_id message
        with
        | Ok _ -> Ok ()
        | Error e -> Error (Error.create_s [%sexp (e : Mailer.Send_error.t)])))
;;

(* What a person would do: find the six digits in the email. *)
let read_code (received : Simulated_inbox.Received.t) =
  String.split_lines received.message.text
  |> List.map ~f:String.strip
  |> List.find_exn ~f:(fun l -> String.length l = 6 && String.for_all l ~f:Char.is_digit)
;;

let main () =
  let time_source =
    Time_source.create ~now:(Time_ns.of_string_with_utc_offset "2026-10-14 09:30:00Z") ()
  in
  let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'x') |> Or_error.ok_exn in
  let codes =
    Codes.create
      ~rng:(Rng.of_seed "example-seed-000001")
      ~time_source
      ~key
      (Otp_store.Memory.create ())
  in
  let inbox =
    Simulated_inbox.create
      ~time_source
      ~rng:(Rng.of_seed "example-seed-000002")
      ~domain:"bank.example"
      ()
  in
  let arrival = Simulated_inbox.next inbox ~to_:customer in
  let%bind () = send_code codes inbox ~locale:Template.Locale.de >>| ok_exn in
  let%bind received = arrival in
  printf "subject: %s\n" received.message.subject;
  let code = read_code received in
  printf "code in the email: %s\n" code;
  let verify code =
    let%map result = Codes.verify codes ~subject:"customer-42" ~purpose:Sign_in ~code in
    print_s [%sexp (result : Otp.Verify_result.t)]
  in
  let%bind () = verify "000000" in
  let%bind () = verify code in
  let%bind () = verify code in
  return ()
;;

let () =
  Command.async ~summary:"sign in with an emailed code" (Command.Param.return main)
  |> Command_unix.run
;;
