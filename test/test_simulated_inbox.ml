open! Core
open! Async
open Otp_mail
open Helpers

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:30:00Z"

let inbox ?dkim () =
  Simulated_inbox.create
    ~time_source:(Time_source.create ~now:t0 ())
    ~rng:(Rng.of_seed "simulated-inbox-seed")
    ~domain:"bank.example"
    ?dkim
    ~outcome:(fun a ->
      if String.equal (Address.domain a) "bounce.example"
      then [ Bounced { reason = "550 5.1.1 mailbox unknown" } ]
      else [ Delivered ])
    ()
;;

let code_message ?(to_ = [ mailbox "zoe@example.org" ]) ?bcc code =
  Message.create
    ~from:(mailbox ~name:"Example Bank" "no-reply@bank.example")
    ~to_
    ?bcc
    ~subject:"Your code"
    ~text:("Your code is " ^ code ^ "\n")
    ()
  |> Or_error.ok_exn
;;

let show_send r =
  print_s [%sexp (r : (Mailer.Delivery_id.t, Mailer.Send_error.t) Result.t)]
;;

let%expect_test "messages land in the inbox and events follow" =
  let t = inbox () in
  let events = Simulated_inbox.events t in
  let%bind r =
    Simulated_inbox.send
      t
      (code_message
         ~to_:[ mailbox "zoe@example.org"; mailbox "nobody@bounce.example" ]
         ~bcc:[ mailbox "audit@bank.example" ]
         "482913")
  in
  show_send r;
  [%expect {| (Ok 3af8619a55d8351142463801d184d18a) |}];
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (Pipe.read_now' events
   |> function
   | `Ok q -> Queue.iter q ~f:(fun e -> print_s [%sexp (e : Mailer.Event.t)])
   | `Eof | `Nothing_available -> print_endline "no events");
  [%expect
    {|
    ((delivery_id 3af8619a55d8351142463801d184d18a) (recipient ()) (status Sent)
     (at "2026-10-14 09:30:00Z"))
    ((delivery_id 3af8619a55d8351142463801d184d18a) (recipient (zoe@example.org))
     (status Delivered) (at "2026-10-14 09:30:00Z"))
    ((delivery_id 3af8619a55d8351142463801d184d18a)
     (recipient (nobody@bounce.example))
     (status (Bounced (reason "550 5.1.1 mailbox unknown")))
     (at "2026-10-14 09:30:00Z"))
    ((delivery_id 3af8619a55d8351142463801d184d18a)
     (recipient (audit@bank.example)) (status Delivered)
     (at "2026-10-14 09:30:00Z"))
    |}];
  let received =
    Simulated_inbox.latest t ~to_:(address "audit@bank.example") |> Option.value_exn
  in
  print_s [%sexp (received.recipients : Address.t list)];
  print_s [%sexp (received.message.to_ : Mailbox.t list)];
  print_s [%sexp (String.is_substring received.wire ~substring:"audit@" : bool)];
  [%expect
    {|
    (zoe@example.org nobody@bounce.example audit@bank.example)
    (((display_name ()) (address zoe@example.org))
     ((display_name ()) (address nobody@bounce.example)))
    false
    |}];
  return ()
;;

let%expect_test "idempotency keys" =
  let t = inbox () in
  let m = code_message "111111" in
  let%bind a = Simulated_inbox.send t ~idempotency_key:"k-1" m in
  let%bind b = Simulated_inbox.send t ~idempotency_key:"k-1" m in
  let%bind c = Simulated_inbox.send t ~idempotency_key:"k-1" (code_message "222222") in
  show_send a;
  show_send b;
  show_send c;
  print_s [%sexp (Simulated_inbox.count t : int)];
  [%expect
    {|
    (Ok 3af8619a55d8351142463801d184d18a)
    (Ok 3af8619a55d8351142463801d184d18a)
    (Error
     (Invalid_message ("idempotency key reused with a different message" k-1)))
    1
    |}];
  return ()
;;

let%expect_test "waiting for the next message and reading a code out of it" =
  let t = inbox () in
  let zoe = address "zoe@example.org" in
  let waiting = Simulated_inbox.next t ~to_:zoe in
  print_s [%sexp (Deferred.is_determined waiting : bool)];
  let%bind (_ : (Mailer.Delivery_id.t, Mailer.Send_error.t) Result.t) =
    Simulated_inbox.send t (code_message "735102")
  in
  let%bind received = waiting in
  let code =
    String.split received.message.text ~on:' '
    |> List.find_map ~f:(fun w ->
      let w = String.strip w in
      Option.some_if (String.length w = 6 && String.for_all w ~f:Char.is_digit) w)
  in
  print_s [%sexp (code : string option)];
  [%expect {|
    false
    (735102)
    |}];
  print_s
    [%sexp
      (List.length (Simulated_inbox.messages t ~subject_contains:"code") : int)
      , (List.length (Simulated_inbox.messages t ~subject_contains:"invoice") : int)];
  Simulated_inbox.clear t;
  print_s [%sexp (Simulated_inbox.count t : int)];
  [%expect {|
    (1 0)
    0
    |}];
  return ()
;;

let%expect_test "messages can be DKIM-signed on the way in" =
  let key =
    Dkim.Private_key.of_pem (In_channel.read_all "fixtures/dkim-test-key.pem")
    |> Or_error.ok_exn
  in
  let t = inbox ~dkim:(Dkim.Config.create ~domain:"bank.example" ~selector:"s1" key) () in
  let%bind r = Simulated_inbox.send t (code_message "482913") in
  let id =
    match r with
    | Ok id -> id
    | Error e -> raise_s [%sexp (e : Mailer.Send_error.t)]
  in
  let received = Simulated_inbox.find t id |> Option.value_exn in
  print_endline (List.hd_exn (String.split_lines received.wire));
  print_s
    [%sexp
      (Dkim.verify (Dkim.Private_key.public_key key) received.wire : unit Or_error.t)];
  [%expect {|
    DKIM-Signature: v=1;
    (Ok ())
    |}];
  return ()
;;
