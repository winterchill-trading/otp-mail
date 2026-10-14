(* Sending through SMTP with STARTTLS and AUTH, against the fake server from
   otp-mail.testing (it listens on 127.0.0.1 and relays nothing). A real relay needs only
   a different [Smtp.Config]: host, port 587, credentials, and no [ca_file] so that the
   system roots are used. *)

open! Core
open! Async
open Otp_mail
module Fake_smtp = Otp_mail_testing.Fake_smtp

let main ~certs () =
  let cert = certs ^/ "localhost.pem", certs ^/ "localhost-key.pem" in
  let%bind server = Fake_smtp.start ~tls:cert ~credentials:("app", "app-password") () in
  let smtp =
    Smtp.create
      ~trace:(fun direction line ->
        printf
          "%s %s\n"
          (match direction with
           | `Client -> "C:"
           | `Server -> "S:")
          line)
      (Smtp.Config.create
         ~host:"localhost"
         ~port:(Fake_smtp.port server)
         ~security:Starttls
         ~credentials:(Smtp.Credentials.create ~username:"app" ~password:"app-password")
         ~ca_file:(certs ^/ "ca.pem")
         ~message_id_domain:"bank.example"
         ())
  in
  let message =
    Message.create
      ~from:
        (Mailbox.create
           ~display_name:"Example Bank"
           (Address.of_string_exn "no-reply@bank.example"))
      ~to_:[ Mailbox.of_address (Address.of_string_exn "zoe@example.org") ]
      ~subject:"Statement ready"
      ~text:"Your October statement is ready in the app.\n"
      ()
    |> Or_error.ok_exn
  in
  let%bind result = Smtp.send smtp message in
  (match result with
   | Ok (_ : Mailer.Delivery_id.t) -> print_endline "accepted"
   | Error e -> print_s [%sexp (e : Mailer.Send_error.t)]);
  List.iter (Fake_smtp.received server) ~f:(fun r ->
    printf
      "server got a message for %s over %s\n"
      (String.concat ~sep:", " r.rcpt_to)
      (if r.tls then "TLS" else "plain TCP"));
  Fake_smtp.close server
;;

let () =
  Command.async
    ~summary:"send one message over STARTTLS to a local fake server"
    (let%map_open.Command certs =
       flag
         "certs"
         (optional_with_default "../test/certs" string)
         ~doc:"DIR test certificates"
     in
     main ~certs)
  |> Command_unix.run
;;
