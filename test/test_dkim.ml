open! Core
open Otp_mail
open Helpers

(* A throwaway 2048-bit key made for these tests with
   [openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048]; it signs nothing else. *)
let key =
  Dkim.Private_key.of_pem (In_channel.read_all "fixtures/dkim-test-key.pem")
  |> Or_error.ok_exn
;;

let public_key = Dkim.Private_key.public_key key

let message =
  Message.create
    ~from:(mailbox ~name:"Example Bank" "no-reply@bank.example")
    ~to_:[ mailbox "zoe@example.org" ]
    ~subject:"Your sign-in code"
    ~text:"Your code is 482913.\n"
    ~html:"<p>Your code is <b>482913</b>.</p>\n"
    ()
  |> Or_error.ok_exn
;;

let wire = Message.render message ~date:fixed_date ~message_id:fixed_message_id
let config = Dkim.Config.create ~domain:"bank.example" ~selector:"mail2026" key

let%expect_test "keys" =
  print_s [%sexp (key : Dkim.Private_key.t)];
  [%expect {| (<redacted> ((rsa_private_key_bits 2048))) |}];
  let pkcs1 =
    Dkim.Private_key.of_pem (In_channel.read_all "fixtures/dkim-test-key-pkcs1.pem")
    |> Or_error.ok_exn
  in
  print_s
    [%sexp
      (String.equal
         (Dkim.Public_key.to_dns_record (Dkim.Private_key.public_key pkcs1))
         (Dkim.Public_key.to_dns_record public_key)
       : bool)];
  [%expect {| true |}];
  let record = Dkim.Public_key.to_dns_record public_key in
  print_endline (String.prefix record 60 ^ "...");
  [%expect {| v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQ... |}];
  let back = Dkim.Public_key.of_dns_record record |> Or_error.ok_exn in
  print_s [%sexp (String.equal (Dkim.Public_key.to_dns_record back) record : bool)];
  [%expect {| true |}];
  print_s [%sexp (Dkim.Private_key.of_pem "not a key" : Dkim.Private_key.t Or_error.t)];
  print_s
    [%sexp (Dkim.Public_key.of_dns_record "v=DKIM1; p=" : Dkim.Public_key.t Or_error.t)];
  [%expect
    {|
    (Error "expected an unencrypted PEM RSA key (BEGIN PRIVATE KEY)")
    (Error "DKIM key has been revoked (empty p=)")
    |}]
;;

let%expect_test "canonicalization (RFC 6376 section 3.4.6 example)" =
  let show canon =
    print_string
      (String.substr_replace_all
         (Dkim.Canonicalization.header canon ~name:"A" ~value:" X"
          ^ Dkim.Canonicalization.header canon ~name:"B " ~value:" Y\t\r\n\tZ  ")
         ~pattern:"\r\n"
         ~with_:"<CRLF>\n");
    print_string
      (String.substr_replace_all
         (Dkim.Canonicalization.body canon " C \r\nD \t E\r\n\r\n\r\n")
         ~pattern:"\r\n"
         ~with_:"<CRLF>\n")
  in
  show Relaxed;
  [%expect {|
    a:X<CRLF>
    b:Y Z<CRLF>
     C<CRLF>
    D E<CRLF>
    |}];
  show Simple;
  [%expect
    {|
    A: X<CRLF>
    B : Y	<CRLF>
    Z  <CRLF>
     C <CRLF>
    D 	 E<CRLF>
    |}];
  print_s
    [%sexp
      ((Dkim.Canonicalization.body Simple "", Dkim.Canonicalization.body Relaxed "")
       : string * string)];
  [%expect {| ("\r\n" "") |}]
;;

let signed config = Dkim.sign config ~now:fixed_date wire |> Or_error.ok_exn

let%expect_test "sign and verify" =
  let s = signed config in
  show_wire (String.prefix s (String.substr_index_exn s ~pattern:"Date:"));
  [%expect
    {|
    DKIM-Signature: v=1;$
     a=rsa-sha256;$
     c=relaxed/relaxed;$
     d=bank.example;$
     s=mail2026;$
     t=1791970200;$
     h=from:subject:date:to:message-id:mime-version:content-type:from;$
     bh=n9t9nCM97uaHsEapUEcnw0BulO99nBxGEUuq9RXByvc=;$
     b=BPu8HCOJPSUbDg5rpMShdMUbZaM/AOI4KuvNThJX8a6VoaxOyXX3NtJW5XlIPmDcslHJOytO$
     jT9BkA+WYMVkUP+3EaidklA3yxbAoet1/Mjl6Xc4T1BNNH7mCJyPUoW/g8iTMHtFehlZFj6x$
     9ZLDIoXVqY3egr7EXv3AUT4LsCiC9bl99jIbxTjxF8L4S7VDm8bUoW/TvdircsICai2+akIU$
     jUqh43cBPYWSgpnYXFJdTtHsiOViBukmcMR/hDxnRoi5pmj1IAu1/SYNd4x6vU+Smx8jn6U7$
     AJZ2Dzvj/3hHfvnSEXu5SbSiPpmDLzjlrUm7acP/78pEYFlkKK2S5A==$
    |}];
  print_s [%sexp (Dkim.verify public_key s : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  List.iter
    [ { config with header_canonicalization = Simple; body_canonicalization = Simple }
    ; { config with header_canonicalization = Relaxed; body_canonicalization = Simple }
    ; { config with expire_after = Some (Time_ns.Span.of_day 7.) }
    ]
    ~f:(fun config ->
      print_s [%sexp (Dkim.verify public_key (signed config) : unit Or_error.t)]);
  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ())
    |}]
;;

let%expect_test "what breaks a signature" =
  let s = signed config in
  let check label tampered =
    printf
      "%-40s %s\n"
      label
      (match Dkim.verify public_key tampered with
       | Ok () -> "verifies"
       | Error e -> Error.to_string_hum e)
  in
  check "unchanged" s;
  check
    "subject changed"
    (String.substr_replace_first s ~pattern:"Subject: Your" ~with_:"Subject: Our");
  check "body changed" (String.substr_replace_first s ~pattern:"482913" ~with_:"000000");
  check "second From added at the top" ("From: attacker@example.net\r\n" ^ s);
  check
    "header re-folded (relaxed)"
    (String.substr_replace_first
       s
       ~pattern:"Subject: Your sign-in"
       ~with_:"Subject:   Your\r\n sign-in");
  check "trailing blank lines added (relaxed)" (s ^ "\r\n\r\n");
  check "unsigned header added" ("X-Spam-Score: 0\r\n" ^ s);
  [%expect
    {|
    unchanged                                verifies
    subject changed                          "signature does not match"
    body changed                             "body hash does not match"
    second From added at the top             "signature does not match"
    header re-folded (relaxed)               verifies
    trailing blank lines added (relaxed)     verifies
    unsigned header added                    verifies
    |}]
;;

let%expect_test "signatures made by dkimpy 1.1.8 with the same key verify" =
  List.iter [ "dkimpy-relaxed.eml"; "dkimpy-simple.eml" ] ~f:(fun name ->
    let raw = In_channel.read_all ("fixtures/" ^ name) in
    print_s [%sexp (name : string), (Dkim.verify public_key raw : unit Or_error.t)]);
  [%expect {|
    (dkimpy-relaxed.eml (Ok ()))
    (dkimpy-simple.eml (Ok ()))
    |}]
;;
