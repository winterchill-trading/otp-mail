open! Core
open Otp_mail
open Helpers

let otp_message ?html () =
  Message.create
    ~from:(mailbox ~name:"Example Bank" "no-reply@bank.example")
    ~to_:[ mailbox ~name:"Zoë Müller" "zoe@example.org" ]
    ~headers:[ "X-Entity-Ref-ID", "otp-7781" ]
    ?html
    ~subject:"Your sign-in code: 482913"
    ~text:"Hello,\n\nYour sign-in code is 482913. It expires in 10 minutes.\n"
    ()
  |> Or_error.ok_exn
;;

let%expect_test "a text-only message" =
  show_wire
    (Message.render (otp_message ()) ~date:fixed_date ~message_id:fixed_message_id);
  [%expect
    {|
    Date: Wed, 14 Oct 2026 09:30:00 +0000$
    From: Example Bank <no-reply@bank.example>$
    To: =?UTF-8?B?Wm/DqyBNw7xsbGVy?= <zoe@example.org>$
    Subject: Your sign-in code: 482913$
    Message-ID: <0123456789abcdef@bank.example>$
    MIME-Version: 1.0$
    X-Entity-Ref-ID: otp-7781$
    Content-Type: text/plain; charset=utf-8$
    Content-Transfer-Encoding: 7bit$
    $
    Hello,$
    $
    Your sign-in code is 482913. It expires in 10 minutes.$
    |}]
;;

let%expect_test "text and HTML alternatives" =
  let html =
    "<p>Hello,</p>\n<p>Your sign-in code is <strong>482913</strong>.</p>\n<p>Grüße</p>\n"
  in
  let wire =
    Message.render (otp_message ~html ()) ~date:fixed_date ~message_id:fixed_message_id
  in
  show_wire wire;
  [%expect
    {|
    Date: Wed, 14 Oct 2026 09:30:00 +0000$
    From: Example Bank <no-reply@bank.example>$
    To: =?UTF-8?B?Wm/DqyBNw7xsbGVy?= <zoe@example.org>$
    Subject: Your sign-in code: 482913$
    Message-ID: <0123456789abcdef@bank.example>$
    MIME-Version: 1.0$
    X-Entity-Ref-ID: otp-7781$
    Content-Type: multipart/alternative;$
     boundary="=_2ccadfe9560eae0d9b2dc6f600f4d0a0"$
    $
    --=_2ccadfe9560eae0d9b2dc6f600f4d0a0$
    Content-Type: text/plain; charset=utf-8$
    Content-Transfer-Encoding: 7bit$
    $
    Hello,$
    $
    Your sign-in code is 482913. It expires in 10 minutes.$
    $
    --=_2ccadfe9560eae0d9b2dc6f600f4d0a0$
    Content-Type: text/html; charset=utf-8$
    Content-Transfer-Encoding: quoted-printable$
    $
    <p>Hello,</p>$
    <p>Your sign-in code is <strong>482913</strong>.</p>$
    <p>Gr=C3=BC=C3=9Fe</p>$
    $
    --=_2ccadfe9560eae0d9b2dc6f600f4d0a0--$
    |}];
  print_s [%sexp (Message.of_wire wire : Message.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((from ((display_name ("Example Bank")) (address no-reply@bank.example)))
      (to_
       (((display_name ("Zo\195\171 M\195\188ller")) (address zoe@example.org))))
      (cc ()) (bcc ()) (reply_to ()) (subject "Your sign-in code: 482913")
      (text
        "Hello,\
       \n\
       \nYour sign-in code is 482913. It expires in 10 minutes.\
       \n")
      (html
       ( "<p>Hello,</p>\
        \n<p>Your sign-in code is <strong>482913</strong>.</p>\
        \n<p>Gr\195\188\195\159e</p>\
        \n"))
      (headers ((X-Entity-Ref-ID otp-7781)))))
    |}]
;;

let%expect_test "validation" =
  let from = mailbox "a@example.org" in
  let to_ = [ mailbox "b@example.org" ] in
  let show r = print_s [%sexp (Result.map r ~f:(fun _ -> ()) : unit Or_error.t)] in
  show (Message.create ~from ~subject:"s" ~text:"t" ());
  show (Message.create ~from ~to_ ~subject:"a\r\nBcc: x@example.org" ~text:"t" ());
  show
    (Message.create
       ~from
       ~to_
       ~headers:[ "Bcc", "x@example.org" ]
       ~subject:"s"
       ~text:"t"
       ());
  show (Message.create ~from ~to_ ~headers:[ "Bad Name", "v" ] ~subject:"s" ~text:"t" ());
  show (Message.create ~from ~to_ ~headers:[ "X-Note", "café" ] ~subject:"s" ~text:"t" ());
  show (Message.create ~from ~bcc:to_ ~subject:"s" ~text:"t" ());
  [%expect
    {|
    (Error "message has no recipients")
    (Error "subject contains a line break")
    (Error ("header is written by the library" Bcc))
    (Error ("invalid header name" "Bad Name"))
    (Error ("header value is not printable ASCII" X-Note))
    (Ok ())
    |}]
;;

let%expect_test "dates" =
  List.iter
    [ "2026-10-14 09:30:00Z"; "2024-02-29 23:59:59.999Z"; "2000-01-01 00:00:00+02:00" ]
    ~f:(fun s ->
      print_endline (Message.rfc5322_date (Time_ns.of_string_with_utc_offset s)));
  [%expect
    {|
    Wed, 14 Oct 2026 09:30:00 +0000
    Thu, 29 Feb 2024 23:59:59 +0000
    Fri, 31 Dec 1999 22:00:00 +0000
    |}]
;;

let read_fixture name = In_channel.read_all ("fixtures/" ^ name)

let%expect_test "fixture: a client-style nested message with LF line breaks" =
  let raw = read_fixture "client-nested.eml" in
  print_s [%sexp (Message.of_wire raw : Message.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((from ((display_name ("Banque Exemple")) (address no-reply@bank.example)))
      (to_
       (((display_name ("Zo\195\171")) (address zoe@example.org))
        ((display_name ("Doe, John")) (address john@example.org))))
      (cc ()) (bcc ()) (reply_to ())
      (subject "Votre code de v\195\169rification")
      (text  "Bonjour Zo\195\171,\
            \nVotre code: 482913 (valable 10 minutes)")
      (html
       ( "<p>Bonjour Zo\195\171,</p>\
        \n<p>Votre code: <b>482913</b></p>\
        \n"))
      (headers
       ((Return-Path <bounce@mail.example>)
        (Received
         "from mx.example (mx.example [192.0.2.10])\tby inbound.example with ESMTPS id abc123\tfor <zoe@example.org>; Wed, 14 Oct 2026 09:30:01 +0000")
        (List-Unsubscribe <mailto:unsubscribe@bank.example>)))))
    |}];
  let entity = Mime.Entity.parse raw |> Or_error.ok_exn in
  let rec outline indent (e : Mime.Entity.t) =
    printf "%s%s\n" indent (Mime.Content_type.mime_type e.content_type);
    match e.body with
    | Leaf content -> printf "%s  %d bytes\n" indent (String.length content)
    | Multipart parts -> List.iter parts ~f:(outline (indent ^ "  "))
  in
  outline "" entity;
  [%expect
    {|
    multipart/mixed
      multipart/alternative
        text/plain
          54 bytes
        text/html
          54 bytes
      application/pdf
        9 bytes
    |}]
;;

let%expect_test "fixture: single 7bit part with CRLF" =
  print_s
    [%sexp (Message.of_wire (read_fixture "single-7bit.eml") : Message.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((from ((display_name ()) (address alerts@bank.example)))
      (to_ (((display_name ()) (address user@example.org)))) (cc ()) (bcc ())
      (reply_to ()) (subject "Plain ASCII notice")
      (text  "Line one\
            \nLine two\
            \n")
      (html ()) (headers ((X-Entity-Ref-ID 7781)))))
    |}]
;;

let%expect_test "fixture: a multipart body without its closing boundary" =
  print_s
    [%sexp
      (Message.of_wire (read_fixture "truncated-multipart.eml") : Message.t Or_error.t)];
  [%expect {| (Error ("multipart: no closing boundary" (boundary b))) |}]
;;

let gen_text =
  let open Quickcheck.Generator.Let_syntax in
  let%bind kind = Int.gen_incl 0 2 in
  match kind with
  | 0 -> String.gen_with_length 40 Char.gen_print
  | 1 ->
    List.gen_with_length
      6
      (Quickcheck.Generator.of_list
         [ "Grüße"
         ; "€ 12,50"
         ; "line\n"
         ; " "
         ; "--=_x"
         ; "."
         ; "code 123456"
         ; String.make 90 'w'
         ; "\t"
         ])
    >>| String.concat
  | _ -> String.quickcheck_generator
;;

let gen_message =
  let open Quickcheck.Generator.Let_syntax in
  let%bind subject =
    gen_text
    >>| String.map ~f:(fun c -> if Char.equal c '\n' || Char.equal c '\r' then ' ' else c)
  and text = gen_text
  and html = Option.quickcheck_generator gen_text
  and name =
    Option.quickcheck_generator
      (Quickcheck.Generator.of_list [ "Zoë"; "Doe, Jane"; "Bank"; "銀行 \"x\"" ])
  and cc = Bool.quickcheck_generator in
  return
    (Message.create
       ~from:(mailbox ?name "no-reply@bank.example")
       ~to_:[ mailbox ?name "to@example.org" ]
       ~cc:(if cc then [ mailbox "cc@example.org"; mailbox "cc2@example.org" ] else [])
       ~bcc:[ mailbox "hidden@example.org" ]
       ~headers:[ "X-Ref", "a  b" ]
       ?html
       ~subject
       ~text
       ()
     |> Or_error.ok_exn)
;;

let without_bcc (m : Message.t) =
  Message.create
    ~from:m.from
    ~to_:m.to_
    ~cc:m.cc
    ?reply_to:m.reply_to
    ~headers:m.headers
    ?html:m.html
    ~subject:m.subject
    ~text:m.text
    ()
  |> Or_error.ok_exn
;;

let%test_unit "of_wire (render m) is m without its Bcc" =
  Quickcheck.test gen_message ~sexp_of:[%sexp_of: Message.t] ~f:(fun m ->
    let wire = Message.render m ~date:fixed_date ~message_id:fixed_message_id in
    [%test_result: Message.t Or_error.t]
      (Message.of_wire wire)
      ~expect:(Ok (without_bcc m));
    assert (not (String.is_substring wire ~substring:"hidden@example.org")))
;;

let%test_unit "rendered lines are 7-bit and at most 998 characters" =
  Quickcheck.test gen_message ~sexp_of:[%sexp_of: Message.t] ~f:(fun m ->
    let wire = Message.render m ~date:fixed_date ~message_id:fixed_message_id in
    assert (String.for_all wire ~f:(fun c -> Char.to_int c < 128));
    List.iter (String.split wire ~on:'\n') ~f:(fun line ->
      assert (String.length line <= 999);
      assert (not (String.mem (String.chop_suffix_if_exists line ~suffix:"\r") '\r'))))
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Message.Stable.V1.bin_shape_t);
  print_endline (Bin_prot.Shape.eval_to_digest_string Message_id.Stable.V1.bin_shape_t);
  [%expect
    {|
    5028adc630193c938cb5568a690621be
    d9a8da25d5656b016fb4dbdc2e4197fb
    |}]
;;
