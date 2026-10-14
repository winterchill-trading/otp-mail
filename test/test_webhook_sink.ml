open! Core
open! Async
open Otp_mail

(* A made-up provider: one event per line, "<delivery id> <status> <recipient>", signed
   with HMAC-SHA256 in an X-Signature header. *)
module Toy_provider = struct
  type t = { key : string }

  let parse t ~headers ~body =
    let open Or_error.Let_syntax in
    let%bind signature =
      match List.Assoc.find headers "X-Signature" ~equal:String.Caseless.equal with
      | Some s -> Ok s
      | None -> Or_error.error_s [%message "unsigned delivery"]
    in
    let%bind () =
      if Webhook_sink.verify_hmac_sha256 ~key:t.key ~body ~signature
      then Ok ()
      else Or_error.error_s [%message "bad signature"]
    in
    String.split_lines body
    |> List.map ~f:(fun line ->
      match String.split line ~on:' ' with
      | [ id; status; recipient ] ->
        let%bind delivery_id = Mailer.Delivery_id.of_string id in
        let%bind recipient = Address.of_string recipient in
        let%map status =
          match status with
          | "delivered" -> Ok Mailer.Status.Delivered
          | "bounced" -> Ok (Mailer.Status.Bounced { reason = "hard bounce" })
          | "complained" -> Ok Mailer.Status.Complained
          | other -> Or_error.error_s [%message "unknown status" other]
        in
        { Mailer.Event.delivery_id
        ; recipient = Some recipient
        ; status
        ; at = Time_ns.of_string_with_utc_offset "2026-10-14 10:00:00Z"
        }
      | _ -> Or_error.error_s [%message "bad line" line])
    |> Or_error.all
  ;;
end

let%expect_test "signed deliveries are published once" =
  let feed = Mailer.Event_feed.create () in
  let events = Mailer.Event_feed.subscribe feed in
  let sink = Webhook_sink.Sink.create feed in
  let provider = { Toy_provider.key = "webhook-secret" } in
  let receive ?signature body =
    let signature =
      Option.value
        signature
        ~default:(Webhook_sink.hmac_sha256_hex ~key:"webhook-secret" body)
    in
    print_s
      [%sexp
        (Webhook_sink.Sink.receive
           sink
           (module Toy_provider)
           provider
           ~headers:[ "x-signature", String.uppercase signature ]
           ~body
         : int Or_error.t)]
  in
  let body = "abc123 delivered zoe@example.org\nabc123 bounced nobody@example.org" in
  receive body;
  receive body;
  receive ~signature:(String.make 64 '0') "abc123 complained zoe@example.org";
  receive "abc123 exploded zoe@example.org";
  [%expect
    {|
    (Ok 2)
    (Ok 0)
    (Error "bad signature")
    (Error ("unknown status" exploded))
    |}];
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (match Pipe.read_now' events with
   | `Ok q ->
     Queue.iter q ~f:(fun (e : Mailer.Event.t) ->
       print_s [%sexp (e.status : Mailer.Status.t)])
   | `Eof | `Nothing_available -> ());
  [%expect {|
    Delivered
    (Bounced (reason "hard bounce"))
    |}];
  return ()
;;

let%expect_test "HMAC (RFC 4231 test case 2)" =
  print_endline (Webhook_sink.hmac_sha256_hex ~key:"Jefe" "what do ya want for nothing?");
  [%expect {| 5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843 |}];
  return ()
;;
