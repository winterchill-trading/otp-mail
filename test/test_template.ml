open! Core
open Otp_mail
module Locale = Template.Locale

let six = Locale.[ en; de; fr; es; it; pt ]

let%expect_test "locales" =
  List.iter
    [ "en"; "pt_br"; "PT-br"; "es-419"; "fil"; "e"; "en-GBR"; "en-US-x" ]
    ~f:(fun s -> print_s [%sexp (s : string), (Locale.of_string s : Locale.t Or_error.t)]);
  [%expect
    {|
    (en (Ok en))
    (pt_br (Ok pt-BR))
    (PT-br (Ok pt-BR))
    (es-419 (Ok es-419))
    (fil (Ok fil))
    (e (Error ("invalid locale" e)))
    (en-GBR (Error ("invalid locale" en-GBR)))
    (en-US-x (Error ("invalid locale" en-US-x)))
    |}]
;;

let%expect_test "resolving a locale" =
  let t = Code_email.template in
  List.iter [ "de"; "de-AT"; "pt-BR"; "ja"; "en-GB" ] ~f:(fun s ->
    printf "%s -> %s\n" s (Locale.to_string (Template.resolve t (Locale.of_string_exn s))));
  [%expect
    {|
    de -> de
    de-AT -> de
    pt-BR -> pt
    ja -> en
    en-GB -> en
    |}]
;;

let%expect_test "the code email passes the catalogue check in six languages" =
  print_s
    [%sexp
      (Template.Catalogue.check ~required_locales:six [ T Code_email.template ]
       : Template.Catalogue.Problem.t list)];
  [%expect {| () |}]
;;

let sample purpose =
  { Code_email.code = "482913"
  ; purpose
  ; expires_in = Time_ns.Span.of_min 10.
  ; service_name = "Example Bank"
  }
;;

let show locale params =
  let r = Template.render Code_email.template ~locale params in
  print_endline r.subject;
  print_endline "---";
  print_string r.text;
  print_endline "---";
  print_string (Option.value_exn r.html)
;;

let%expect_test "English sign-in" =
  show Locale.en (sample Sign_in);
  [%expect
    {|
    Your Example Bank sign-in code
    ---
    Your sign-in code is:

        482913

    It expires in 10 minutes.
    If you did not try to sign in, you can ignore this email; someone may have typed your address by mistake.

    Never share this code. Example Bank staff will never ask you for it.
    ---
    <p>Your sign-in code is:</p>
    <p style="font-size:28px;font-weight:bold;letter-spacing:6px">482913</p>
    <p>It expires in 10 minutes. If you did not try to sign in, you can ignore this email; someone may have typed your address by mistake.</p>
    <p style="color:#555">Never share this code. Example Bank staff will never ask you for it.</p>
    |}]
;;

let%expect_test "every language, payment confirmation subject and first line" =
  List.iter six ~f:(fun locale ->
    let r =
      Template.render
        Code_email.template
        ~locale
        (sample (Payment { amount = "EUR 25.00"; payee = "Acme Ltd" }))
    in
    printf
      "%s | %s | %s\n"
      (Locale.to_string locale)
      r.subject
      (List.hd_exn (String.split_lines r.text)));
  [%expect
    {|
    en | Confirm your payment of EUR 25.00 to Acme Ltd | Code to confirm a payment of EUR 25.00 to Acme Ltd:
    de | Bestätigen Sie Ihre Zahlung von EUR 25.00 an Acme Ltd | Code zur Bestätigung einer Zahlung von EUR 25.00 an Acme Ltd:
    fr | Confirmez votre paiement de EUR 25.00 à Acme Ltd | Code de confirmation du paiement de EUR 25.00 à Acme Ltd :
    es | Confirma tu pago de EUR 25.00 a Acme Ltd | Código para confirmar un pago de EUR 25.00 a Acme Ltd:
    it | Conferma il pagamento di EUR 25.00 a Acme Ltd | Codice per confermare un pagamento di EUR 25.00 a Acme Ltd:
    pt | Confirme o seu pagamento de EUR 25.00 a Acme Ltd | Código para confirmar um pagamento de EUR 25.00 a Acme Ltd:
    |}]
;;

let%expect_test "expiry rounds up to whole minutes, singular and plural" =
  List.iter [ 30.; 60.; 61.; 600. ] ~f:(fun s ->
    let r =
      Template.render
        Code_email.template
        ~locale:Locale.de
        { (sample Sign_in) with expires_in = Time_ns.Span.of_sec s }
    in
    print_endline (List.nth_exn (String.split_lines r.text) 4));
  [%expect
    {|
    Der Code ist 1 Minute gültig.
    Der Code ist 1 Minute gültig.
    Der Code ist 2 Minuten gültig.
    Der Code ist 10 Minuten gültig.
    |}]
;;

let%expect_test "the catalogue check finds broken templates" =
  let broken =
    Template.create
      ~name:"broken"
      ~samples:[ "<b>&" ]
      ~must_mention:(fun p -> [ p ])
      [ ( Locale.en
        , fun p ->
            { subject = "Hello\nBcc: x"
            ; text = "no mention"
            ; html = Some ("<p>" ^ p ^ "<div></p>")
            } )
      ; (Locale.de, fun _ -> failwith "not translated yet")
      ; ( Locale.fr
        , fun p ->
            { subject = ""
            ; text = p
            ; html = Some ("<p>" ^ Template.Html.escape p ^ "</p>")
            } )
      ]
  in
  let duplicate =
    Template.create
      ~name:"broken"
      ~samples:[ () ]
      [ (Locale.en, fun () -> { subject = "s"; text = "t"; html = None }) ]
  in
  List.iter
    (Template.Catalogue.check
       ~required_locales:[ Locale.en; Locale.it ]
       [ T broken; T duplicate ])
    ~f:(fun p -> print_s [%sexp (p : Template.Catalogue.Problem.t)]);
  [%expect
    {|
    ((template broken) (locale ()) (problem "name used twice"))
    ((template broken) (locale (de))
     (problem "renderer raised: (Failure \"not translated yet\")"))
    ((template broken) (locale (en)) (problem "</p> closes <div>"))
    ((template broken) (locale (en))
     (problem "HTML contains \"<b>&\" unescaped"))
    ((template broken) (locale (en)) (problem "HTML does not mention \"<b>&\""))
    ((template broken) (locale (en)) (problem "line break in subject"))
    ((template broken) (locale (en)) (problem "text does not mention \"<b>&\""))
    ((template broken) (locale (fr)) (problem "empty subject"))
    ((template broken) (locale (it)) (problem "missing locale"))
    |}]
;;

let%expect_test "rendering into a message" =
  let m =
    Template.to_message
      Code_email.template
      ~locale:(Locale.of_string_exn "fr-CA")
      (sample Passcode_reset)
      ~from:
        (Mailbox.create
           ~display_name:"Example Bank"
           (Address.of_string_exn "no-reply@bank.example"))
      ~to_:[ Mailbox.of_address (Address.of_string_exn "zoe@example.org") ]
      ()
    |> Or_error.ok_exn
  in
  print_endline m.subject;
  [%expect {| Votre code de réinitialisation du code d'accès Example Bank |}]
;;

let%expect_test "stable digest" =
  print_endline (Bin_prot.Shape.eval_to_digest_string Locale.Stable.V1.bin_shape_t);
  [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
;;
