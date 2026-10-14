open! Core
open Otp_mail
module Qp = Mime.Quoted_printable

let show_qp s =
  print_endline
    (String.substr_replace_all (Qp.encode s) ~pattern:"\r\n" ~with_:"<CRLF>\n")
;;

let%expect_test "quoted-printable keeps ASCII and escapes the rest" =
  show_qp "Hello, world";
  [%expect {| Hello, world |}];
  show_qp "Grüße = 3 € ";
  [%expect {| Gr=C3=BC=C3=9Fe =3D 3 =E2=82=AC=20 |}];
  show_qp "line one\nline two \r\nlast\tline\t";
  [%expect {|
    line one<CRLF>
    line two=20<CRLF>
    last	line=09
    |}]
;;

let%expect_test "quoted-printable wraps long lines with soft breaks" =
  show_qp (String.make 80 'a' ^ " " ^ String.make 10 'b');
  [%expect
    {|
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=<CRLF>
    aaaaa bbbbbbbbbb
    |}];
  (* An escape is never split across a soft break. *)
  show_qp (String.make 73 'a' ^ "é");
  [%expect
    {|
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=<CRLF>
    =C3=A9
    |}]
;;

let%expect_test "quoted-printable decoding" =
  let show s = print_s [%sexp (Qp.decode s : string Or_error.t)] in
  show "soft=\r\nbreak and trailing   \r\nhard";
  [%expect {|
    (Ok  "softbreak and trailing\r\
        \nhard")
    |}];
  show "lower =c3=a9 case";
  [%expect {| (Ok "lower \195\169 case") |}];
  show "bad =ZZ";
  [%expect {| (Error ("quoted-printable: bad escape" (line "bad =ZZ"))) |}];
  show "cut =4";
  [%expect {| (Error ("quoted-printable: bad escape" (line "cut =4"))) |}]
;;

let crlf_normalise s =
  let b = Buffer.create (String.length s) in
  String.iteri s ~f:(fun i c ->
    if Char.equal c '\n' && not (i > 0 && Char.equal s.[i - 1] '\r')
    then Buffer.add_string b "\r\n"
    else Buffer.add_char b c);
  Buffer.contents b
;;

let%test_unit "quoted-printable binary mode round-trips any bytes" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    [%test_result: string Or_error.t]
      (Qp.decode (Qp.encode ~binary:true s))
      ~expect:(Ok s))
;;

let%test_unit "quoted-printable text mode round-trips up to CRLF line breaks" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    [%test_result: string Or_error.t]
      (Qp.decode (Qp.encode s))
      ~expect:(Ok (crlf_normalise s)))
;;

let%test_unit "quoted-printable lines stay within 76 characters of 7-bit text" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    List.iter
      (String.split (Qp.encode s) ~on:'\n')
      ~f:(fun line ->
        let line = String.chop_suffix_if_exists line ~suffix:"\r" in
        assert (String.length line <= 76);
        assert (
          String.for_all line ~f:(fun c ->
            (Char.to_int c >= 32 && Char.to_int c < 127) || Char.equal c '\t'))))
;;

let%expect_test "base64" =
  List.iter [ ""; "f"; "fo"; "foo"; "foob"; "fooba"; "foobar" ] ~f:(fun s ->
    print_endline (Mime.Base64.encode s));
  [%expect {|
    Zg==
    Zm8=
    Zm9v
    Zm9vYg==
    Zm9vYmE=
    Zm9vYmFy
    |}];
  print_endline
    (String.substr_replace_all
       (Mime.Base64.encode (String.make 60 'x'))
       ~pattern:"\r\n"
       ~with_:"<CRLF>\n");
  [%expect
    {|
    eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4<CRLF>
    eHh4
    |}];
  print_s [%sexp (Mime.Base64.decode "Zm9v\r\nYmFy" : string Or_error.t)];
  [%expect {| (Ok foobar) |}];
  print_s [%sexp (Mime.Base64.decode "Zm9v!mFy" : string Or_error.t)];
  [%expect {| (Error ("base64: bad character" (at 4))) |}];
  print_s [%sexp (Mime.Base64.decode "Zm9" : string Or_error.t)];
  [%expect {| (Error ("base64: length is not a multiple of 4" (n 3))) |}]
;;

let%test_unit "base64 round-trips" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 String.quickcheck_generator (Int.gen_incl 0 100))
    ~sexp_of:[%sexp_of: string * int]
    ~f:(fun (s, line_length) ->
      [%test_result: string Or_error.t]
        (Mime.Base64.decode (Mime.Base64.encode ~line_length s))
        ~expect:(Ok s))
;;

let%expect_test "encoded words" =
  let show s =
    let e = Mime.Encoded_word.encode s in
    print_endline e;
    print_endline (Mime.Encoded_word.decode e)
  in
  show "Plain ASCII";
  [%expect {|
    Plain ASCII
    Plain ASCII
    |}];
  show "Zoë Müller";
  [%expect {|
    =?UTF-8?B?Wm/DqyBNw7xsbGVy?=
    Zoë Müller
    |}];
  show "Ваш код подтверждения для входа в приложение";
  [%expect
    {|
    =?UTF-8?B?0JLQsNGIINC60L7QtCDQv9C+0LTRgtCy0LXRgNC20LTQtdC90LjRjyDQtNC7?= =?UTF-8?B?0Y8g0LLRhdC+0LTQsCDQsiDQv9GA0LjQu9C+0LbQtdC90LjQtQ==?=
    Ваш код подтверждения для входа в приложение
    |}];
  print_endline
    (Mime.Encoded_word.decode
       "=?iso-8859-1?q?caf=E9_cr=E8me?= and =?US-ASCII*en?Q?more?=");
  [%expect {| café crème and more |}];
  print_endline (Mime.Encoded_word.decode "=?utf-8?x?bad?= stays");
  [%expect {| =?utf-8?x?bad?= stays |}]
;;

let%test_unit "encoded words round-trip and stay within 75 characters" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:[%sexp_of: string] ~f:(fun s ->
    let e = Mime.Encoded_word.encode s in
    if Mime.Encoded_word.needs_encoding s
    then List.iter (String.split e ~on:' ') ~f:(fun w -> assert (String.length w <= 75));
    [%test_result: string] (Mime.Encoded_word.decode e) ~expect:s)
;;

let%expect_test "header folding" =
  let subject =
    "Your one-time code for signing in expires in ten minutes, so please use it soon"
  in
  print_string
    (String.substr_replace_all
       (Mime.Header.fold ~name:"Subject" subject)
       ~pattern:"\r\n"
       ~with_:"<CRLF>\n");
  [%expect
    {|
    Subject: Your one-time code for signing in expires in ten minutes, so please<CRLF>
     use it soon<CRLF>
    |}];
  print_string (Mime.Header.fold ~name:"X-Injected" "a\r\nBcc: victim@example.com");
  [%expect {| X-Injected: a  Bcc: victim@example.com |}]
;;

let%test_unit "folding then unfolding gives the field back" =
  Quickcheck.test
    (String.gen_with_length 200 (Quickcheck.Generator.of_list [ 'a'; 'b'; ' '; '=' ]))
    ~sexp_of:[%sexp_of: string]
    ~f:(fun value ->
      let folded = Mime.Header.fold ~name:"X-Test" value in
      [%test_result: string]
        (Mime.Header.unfold folded)
        ~expect:("X-Test: " ^ value ^ "\r\n"))
;;

let%expect_test "header parsing" =
  let raw = "From: a@example.com\r\nSubject: one\r\n two\r\nX-Empty:\r\n\r\nbody" in
  print_s [%sexp (Mime.Header.parse raw : Mime.Header.t Or_error.t)];
  [%expect {| (Ok ((From a@example.com) (Subject "one two") (X-Empty ""))) |}];
  print_s [%sexp (Mime.Header.parse "no colon here" : Mime.Header.t Or_error.t)];
  [%expect {| (Error ("header: line without a colon" "no colon here")) |}]
;;

let%expect_test "content types" =
  let show s =
    match Mime.Content_type.parse s with
    | Ok t -> print_endline (Mime.Content_type.to_string t)
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  show "Text/Plain; Charset=\"utf-8\"";
  [%expect {| text/plain; charset=utf-8 |}];
  show "multipart/alternative; boundary=\"=_a b\"";
  [%expect {| multipart/alternative; boundary="=_a b" |}];
  show "text";
  [%expect {| ("content-type: cannot parse" text) |}]
;;
