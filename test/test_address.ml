open! Core
open Otp_mail

let%expect_test "valid addresses" =
  List.iter
    [ "user@example.com"
    ; "first.last+tag@Sub.Example.COM"
    ; "\"first last\"@example.com"
    ; "\"quote\\\"inside\"@example.com"
    ; "!#$%&'*+-/=?^_`{}|~@example.com"
    ; "user@[192.0.2.1]"
    ; "a@b"
    ]
    ~f:(fun s ->
      match Address.of_string s with
      | Ok a ->
        printf
          "%-36s local=%s domain=%s\n"
          (Address.to_string a)
          (Address.local_part a)
          (Address.domain a)
      | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    user@example.com                     local=user domain=example.com
    first.last+tag@sub.example.com       local=first.last+tag domain=sub.example.com
    "first last"@example.com             local="first last" domain=example.com
    "quote\"inside"@example.com          local="quote\"inside" domain=example.com
    !#$%&'*+-/=?^_`{}|~@example.com      local=!#$%&'*+-/=?^_`{}|~ domain=example.com
    user@[192.0.2.1]                     local=user domain=[192.0.2.1]
    a@b                                  local=a domain=b
    |}]
;;

let%expect_test "invalid addresses" =
  List.iter
    [ ""
    ; "plain"
    ; "@example.com"
    ; "user@"
    ; "two..dots@example.com"
    ; ".lead@example.com"
    ; "trail.@example.com"
    ; "sp ace@example.com"
    ; " user@example.com"
    ; "user@exa mple.com"
    ; "user@example..com"
    ; "\"unterminated@example.com"
    ; "user\r\n@example.com"
    ; "ünïcode@example.com"
    ; String.make 65 'a' ^ "@example.com"
    ; "a@" ^ String.make 260 'b'
    ]
    ~f:(fun s ->
      match Address.of_string s with
      | Ok a -> print_endline ("accepted " ^ Address.to_string a)
      | Error e ->
        let reason =
          match Error.sexp_of_t e with
          | Sexp.List [ _; Atom reason; _ ] -> reason
          | other -> Sexp.to_string other
        in
        print_endline reason);
  [%expect
    {|
    no @
    no @
    empty local part
    empty domain
    bad local part
    bad local part
    bad local part
    bad local part
    bad local part
    bad domain
    bad domain
    bad local part
    bad local part
    bad local part
    local part longer than 64 octets
    longer than 254 octets
    |}]
;;

let%expect_test "redacted form for logs" =
  print_endline (Address.redacted (Address.of_string_exn "jane.doe@example.com"));
  [%expect {| j***@example.com |}]
;;

let%expect_test "sexp and bin_io validate on read" =
  print_s [%sexp (Address.of_string_exn "a@Example.com" : Address.t)];
  [%expect {| a@example.com |}];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Address.t_of_sexp (Sexp.Atom "not an address"))
       : Address.t Or_error.t)];
  [%expect
    {|
    (Error
     (Of_sexp_error "(\"invalid email address\" \"no @\" \"not an address\")"
      (invalid_sexp "not an address")))
    |}];
  let bad = Bin_prot.Writer.to_string String.bin_writer_t "nope" in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Bin_prot.Reader.of_string Address.Stable.V1.bin_reader_t bad)
       : Address.t Or_error.t)];
  [%expect {| (Error ("invalid email address" "no @" nope)) |}]
;;

let%expect_test "stable digests" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Address.Stable.V1.bin_shape_t);
  print_endline (Bin_prot.Shape.eval_to_digest_string Mailbox.Stable.V1.bin_shape_t);
  [%expect
    {|
    d9a8da25d5656b016fb4dbdc2e4197fb
    525747a1d2381bd11958dae5f670d83c
    |}]
;;

let%expect_test "mailboxes in headers" =
  let a = Address.of_string_exn "no-reply@bank.example" in
  List.iter
    [ None
    ; Some "Example Bank"
    ; Some "Bank, Support"
    ; Some "Say \"hi\""
    ; Some "Zoë's Bank"
    ; Some "Evil\r\nBcc: x@example.com"
    ; Some "   "
    ]
    ~f:(fun display_name ->
      let m = Mailbox.create ?display_name a in
      let s = Mailbox.to_string m in
      let back = Mailbox.of_string s |> Or_error.ok_exn in
      printf "%s  [round trip: %b]\n" s (Mailbox.equal m back));
  [%expect
    {|
    no-reply@bank.example  [round trip: true]
    Example Bank <no-reply@bank.example>  [round trip: true]
    "Bank, Support" <no-reply@bank.example>  [round trip: true]
    "Say \"hi\"" <no-reply@bank.example>  [round trip: true]
    =?UTF-8?B?Wm/DqydzIEJhbms=?= <no-reply@bank.example>  [round trip: true]
    "Evil  Bcc: x@example.com" <no-reply@bank.example>  [round trip: true]
    no-reply@bank.example  [round trip: true]
    |}]
;;

let%expect_test "mailbox lists" =
  let show s = print_s [%sexp (Mailbox.list_of_string s : Mailbox.t list Or_error.t)] in
  show "a@example.com, \"Doe, Jane\" <jane@example.com>,B <b@example.com>";
  [%expect
    {|
    (Ok
     (((display_name ()) (address a@example.com))
      ((display_name ("Doe, Jane")) (address jane@example.com))
      ((display_name (B)) (address b@example.com))))
    |}];
  show "a@example.com, broken";
  [%expect {| (Error ("invalid email address" "no @" broken)) |}]
;;

let gen_display_name =
  Quickcheck.Generator.union
    [ String.gen_with_length 8 Char.gen_print
    ; Quickcheck.Generator.of_list [ "Zoë"; "Ñandú Ltd"; "銀行"; "a \"b\" \\c" ]
    ]
;;

let%test_unit "mailboxes round-trip through their header form" =
  Quickcheck.test gen_display_name ~sexp_of:[%sexp_of: string] ~f:(fun display_name ->
    let m = Mailbox.create ~display_name (Address.of_string_exn "x@example.com") in
    [%test_result: Mailbox.t Or_error.t]
      (Mailbox.of_string (Mailbox.to_string m))
      ~expect:(Ok m))
;;
