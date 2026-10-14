open! Core
open! Async
open Otp_mail
open Helpers
module Fake = Otp_mail_testing.Fake_smtp

(* certs/ holds a throwaway CA (its key was deleted after signing) and two server
   certificates it signed, for "localhost" and for "other.example". *)
let localhost_cert = "certs/localhost.pem", "certs/localhost-key.pem"
let other_cert = "certs/other.example.pem", "certs/other.example-key.pem"
let ca_file = "certs/ca.pem"

let message =
  Message.create
    ~from:(mailbox ~name:"Example Bank" "no-reply@bank.example")
    ~to_:[ mailbox ~name:"Zoë" "zoe@example.org"; mailbox "nobody@example.org" ]
    ~bcc:[ mailbox "audit@bank.example" ]
    ~subject:"Your sign-in code"
    ~text:"Your code is 482913.\n.\n..leading dots survive\n"
    ~html:"<p>Your code is <b>482913</b>.</p>\n"
    ()
  |> Or_error.ok_exn
;;

let client
  ?(security = Smtp.Security.Plain)
  ?credentials
  ?(allow_plaintext_auth = false)
  ?(ca_file = ca_file)
  ?(trace = false)
  fake
  =
  Smtp.create
    ~rng:(Rng.of_seed "smtp-test-seed-0001")
    ~trace:(fun dir line ->
      if trace
      then
        printf
          "%s %s\n"
          (match dir with
           | `Client -> "C:"
           | `Server -> "S:")
          line)
    (Smtp.Config.create
       ~host:"localhost"
       ~port:(Fake.port fake)
       ~security
       ?credentials
       ~allow_plaintext_auth
       ~ca_file
       ~timeout:(Time_ns.Span.of_sec 10.)
       ~message_id_domain:"bank.example"
       ())
;;

(* TLS error texts differ between OpenSSL builds, so only say which kind of error it was. *)
let show_result (r : (Mailer.Delivery_id.t, Mailer.Send_error.t) Result.t) =
  match r with
  | Ok _ -> print_endline "sent"
  | Error (Transport e) ->
    let s = Error.to_string_hum e in
    if String.is_prefix s ~prefix:"(TLS"
    then
      print_endline
        (if String.is_substring s ~substring:"hostname did not match"
         then "transport error: TLS, host name does not match the certificate"
         else if String.is_substring s ~substring:"certificate verify failed"
         then "transport error: TLS, certificate chain not trusted"
         else "transport error: TLS handshake failed")
    else printf "transport error: %s\n" s
  | Error e -> print_s [%sexp (e : Mailer.Send_error.t)]
;;

let show_received fake =
  List.iter (Fake.received fake) ~f:(fun (r : Fake.Received.t) ->
    print_s
      [%sexp
        { mail_from : string = r.mail_from
        ; rcpt_to : string list = r.rcpt_to
        ; tls : bool = r.tls
        ; authenticated_as : string option = r.authenticated_as
        }])
;;

let%expect_test "a plain session, as with a relay on localhost" =
  let%bind fake = Fake.start () in
  let smtp = client ~trace:true fake in
  let%bind r = Smtp.send smtp message in
  show_result r;
  [%expect
    {|
    S: 220 fake.smtp.invalid ESMTP ready
    C: EHLO localhost
    S: 250-fake.smtp.invalid
    S: 250 SIZE 10485760
    C: MAIL FROM:<no-reply@bank.example>
    S: 250 sender ok
    C: RCPT TO:<zoe@example.org>
    S: 250 recipient ok
    C: RCPT TO:<nobody@example.org>
    S: 250 recipient ok
    C: RCPT TO:<audit@bank.example>
    S: 250 recipient ok
    C: DATA
    S: 354 end data with <CR><LF>.<CR><LF>
    C: <message data>
    S: 250 queued as FAKE1
    C: QUIT
    S: 221 bye
    sent
    |}];
  show_received fake;
  let data = (List.hd_exn (Fake.received fake)).data in
  print_s
    [%sexp
      (Result.map (Message.of_wire data) ~f:(fun m ->
         Message.equal
           m
           (Message.of_wire
              (Message.render message ~date:fixed_date ~message_id:fixed_message_id)
            |> Or_error.ok_exn))
       : bool Or_error.t)];
  [%expect
    {|
    ((mail_from no-reply@bank.example)
     (rcpt_to (zoe@example.org nobody@example.org audit@bank.example))
     (tls false) (authenticated_as ()))
    (Ok true)
    |}];
  Fake.close fake
;;

let%expect_test "STARTTLS with AUTH PLAIN" =
  let%bind fake = Fake.start ~tls:localhost_cert ~credentials:("app", "s3cret") () in
  let smtp =
    client
      ~security:Starttls
      ~credentials:(Smtp.Credentials.create ~username:"app" ~password:"s3cret")
      ~trace:true
      fake
  in
  let%bind r = Smtp.send smtp message in
  show_result r;
  show_received fake;
  print_s [%sexp (Fake.transcript fake : string list)];
  [%expect
    {|
    S: 220 fake.smtp.invalid ESMTP ready
    C: EHLO localhost
    S: 250-fake.smtp.invalid
    S: 250-STARTTLS
    S: 250-AUTH PLAIN LOGIN
    S: 250 SIZE 10485760
    C: STARTTLS
    S: 220 ready to start TLS
    C: EHLO localhost
    S: 250-fake.smtp.invalid
    S: 250-AUTH PLAIN LOGIN
    S: 250 SIZE 10485760
    C: AUTH PLAIN <redacted>
    S: 235 authenticated
    C: MAIL FROM:<no-reply@bank.example>
    S: 250 sender ok
    C: RCPT TO:<zoe@example.org>
    S: 250 recipient ok
    C: RCPT TO:<nobody@example.org>
    S: 250 recipient ok
    C: RCPT TO:<audit@bank.example>
    S: 250 recipient ok
    C: DATA
    S: 354 end data with <CR><LF>.<CR><LF>
    C: <message data>
    S: 250 queued as FAKE1
    C: QUIT
    S: 221 bye
    sent
    ((mail_from no-reply@bank.example)
     (rcpt_to (zoe@example.org nobody@example.org audit@bank.example)) (tls true)
     (authenticated_as (app)))
    ("EHLO localhost" STARTTLS "EHLO localhost" "AUTH PLAIN <redacted>"
     "MAIL FROM:<no-reply@bank.example>" "RCPT TO:<zoe@example.org>"
     "RCPT TO:<nobody@example.org>" "RCPT TO:<audit@bank.example>" DATA QUIT)
    |}];
  Fake.close fake
;;

let%expect_test "AUTH LOGIN when PLAIN is not offered" =
  let%bind fake =
    Fake.start
      ~tls:localhost_cert
      ~credentials:("app", "s3cret")
      ~auth_mechanisms:[ "LOGIN" ]
      ()
  in
  let smtp =
    client
      ~security:Starttls
      ~credentials:(Smtp.Credentials.create ~username:"app" ~password:"s3cret")
      fake
  in
  let%bind r = Smtp.send smtp message in
  show_result r;
  show_received fake;
  [%expect
    {|
    sent
    ((mail_from no-reply@bank.example)
     (rcpt_to (zoe@example.org nobody@example.org audit@bank.example)) (tls true)
     (authenticated_as (app)))
    |}];
  let wrong =
    client
      ~security:Starttls
      ~credentials:(Smtp.Credentials.create ~username:"app" ~password:"guess")
      fake
  in
  let%bind r = Smtp.send wrong message in
  show_result r;
  [%expect {| (Rejected (code 535) (message "535 authentication failed")) |}];
  Fake.close fake
;;

let%expect_test "implicit TLS" =
  let%bind fake = Fake.start ~tls:localhost_cert ~implicit_tls:true () in
  let%bind r = Smtp.send (client ~security:Implicit_tls fake) message in
  show_result r;
  show_received fake;
  [%expect
    {|
    sent
    ((mail_from no-reply@bank.example)
     (rcpt_to (zoe@example.org nobody@example.org audit@bank.example)) (tls true)
     (authenticated_as ()))
    |}];
  Fake.close fake
;;

let%expect_test "TLS refuses a certificate for another host, or from an unknown CA" =
  let%bind fake = Fake.start ~tls:other_cert () in
  let%bind r = Smtp.send (client ~security:Starttls fake) message in
  show_result r;
  [%expect {| transport error: TLS, host name does not match the certificate |}];
  let%bind () = Fake.close fake in
  let%bind fake = Fake.start ~tls:localhost_cert () in
  let%bind r =
    Smtp.send (client ~security:Starttls ~ca_file:"certs/other.example.pem" fake) message
  in
  show_result r;
  print_s [%sexp (List.length (Fake.received fake) : int)];
  [%expect {|
    transport error: TLS, certificate chain not trusted
    0
    |}];
  Fake.close fake
;;

let%expect_test "no downgrade: STARTTLS required but not offered" =
  let%bind fake = Fake.start ~tls:localhost_cert ~advertise_starttls:false () in
  let%bind r = Smtp.send (client ~security:Starttls fake) message in
  show_result r;
  print_s [%sexp (Fake.transcript fake : string list)];
  [%expect
    {|
    transport error: the SMTP server does not offer STARTTLS; refusing to continue
    ("EHLO localhost")
    |}];
  Fake.close fake
;;

let%expect_test "no credentials over a plain connection unless allowed" =
  let%bind fake = Fake.start ~credentials:("app", "s3cret") () in
  let credentials = Smtp.Credentials.create ~username:"app" ~password:"s3cret" in
  let%bind r = Smtp.send (client ~credentials fake) message in
  show_result r;
  let%bind r = Smtp.send (client ~credentials ~allow_plaintext_auth:true fake) message in
  show_result r;
  [%expect
    {|
    transport error: refusing to send SMTP credentials without TLS
    sent
    |}];
  Fake.close fake
;;

let%expect_test "refusals map to send errors and events" =
  let%bind fake =
    Fake.start
      ~recipient_reply:(fun a ->
        if String.equal a "nobody@example.org"
        then Some (550, "5.1.1 no such user")
        else None)
      ()
  in
  let smtp = client fake in
  let events = Smtp.events smtp in
  let%bind r = Smtp.send smtp message in
  show_result r;
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (match Pipe.read_now' events with
   | `Ok q ->
     Queue.iter q ~f:(fun (e : Mailer.Event.t) ->
       print_s [%sexp (e.recipient : Address.t option), (e.status : Mailer.Status.t)])
   | `Eof | `Nothing_available -> ());
  show_received fake;
  [%expect
    {|
    sent
    (() Sent)
    ((nobody@example.org) (Failed (reason "550 5.1.1 no such user")))
    ((mail_from no-reply@bank.example)
     (rcpt_to (zoe@example.org audit@bank.example)) (tls false)
     (authenticated_as ()))
    |}];
  let only_nobody =
    Message.create
      ~from:message.from
      ~to_:[ mailbox "nobody@example.org" ]
      ~subject:"s"
      ~text:"t"
      ()
    |> Or_error.ok_exn
  in
  let%bind r = Smtp.send smtp only_nobody in
  show_result r;
  [%expect {| (Rejected (code 550) (message "550 5.1.1 no such user")) |}];
  let%bind () = Fake.close fake in
  let try_with ?data_reply ?greeting () =
    let%bind fake = Fake.start ?data_reply ?greeting () in
    let%bind r = Smtp.send (client fake) message in
    show_result r;
    Fake.close fake
  in
  let%bind () = try_with ~data_reply:(451, "4.3.0 try again later") () in
  let%bind () = try_with ~data_reply:(554, "5.7.1 rejected as spam") () in
  let%bind () = try_with ~greeting:(554, "no service") () in
  [%expect
    {|
    (Temporary (message "451 4.3.0 try again later"))
    (Rejected (code 554) (message "554 5.7.1 rejected as spam"))
    (Rejected (code 554) (message "554 no service"))
    |}];
  return ()
;;

let%expect_test "a closed port is a transport error" =
  let%bind fake = Fake.start () in
  let smtp = client fake in
  let%bind () = Fake.close fake in
  let%bind r = Smtp.send smtp message in
  (match r with
   | Error (Transport _) -> print_endline "transport error"
   | _ -> show_result r);
  [%expect {| transport error |}];
  return ()
;;

let%expect_test "dot stuffing" =
  print_string
    (String.substr_replace_all
       (Smtp.dot_stuff ".a\r\nb\n..c\r\n")
       ~pattern:"\r\n"
       ~with_:"<CRLF>\n");
  print_string
    (String.substr_replace_all
       (Smtp.dot_stuff "no final break")
       ~pattern:"\r\n"
       ~with_:"<CRLF>\n");
  [%expect
    {|
    ..a<CRLF>
    b<CRLF>
    ...c<CRLF>
    .<CRLF>
    no final break<CRLF>
    .<CRLF>
    |}];
  return ()
;;
