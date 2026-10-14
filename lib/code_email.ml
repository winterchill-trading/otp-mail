open! Core

type purpose =
  | Sign_in
  | Passcode_reset
  | Payment of
      { amount : string
      ; payee : string
      }
[@@deriving sexp_of]

type params =
  { code : string
  ; purpose : purpose
  ; expires_in : Time_ns.Span.t
  ; service_name : string
  }
[@@deriving sexp_of]

let minutes span = Float.iround_up_exn (Time_ns.Span.to_min span) |> Int.max 1

(* The words of one language. [heading] is the subject, [intro] the line before the code,
   [ignore] what to do if the reader did not ask for it. *)
type words =
  { heading : params -> string
  ; intro : params -> string
  ; expiry : int -> string
  ; ignore : params -> string
  ; never_share : params -> string
  }

let plural n ~one ~many = if n = 1 then one else many

let en =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "Your %s sign-in code" p.service_name
        | Passcode_reset -> sprintf "Your %s passcode reset code" p.service_name
        | Payment { amount; payee } ->
          sprintf "Confirm your payment of %s to %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Your sign-in code is:"
        | Passcode_reset -> "Use this code to reset your passcode:"
        | Payment { amount; payee } ->
          sprintf "Code to confirm a payment of %s to %s:" amount payee)
  ; expiry =
      (fun n -> sprintf "It expires in %d %s." n (plural n ~one:"minute" ~many:"minutes"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in ->
          "If you did not try to sign in, you can ignore this email; someone may have \
           typed your address by mistake."
        | Passcode_reset ->
          "If you did not ask to reset your passcode, do not use the code and contact us."
        | Payment _ -> "Only enter it if you made this payment yourself.")
  ; never_share =
      (fun p ->
        sprintf
          "Never share this code. %s staff will never ask you for it."
          p.service_name)
  }
;;

let de =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "Ihr Anmeldecode für %s" p.service_name
        | Passcode_reset ->
          sprintf "Ihr Code zum Zurücksetzen des Passcodes für %s" p.service_name
        | Payment { amount; payee } ->
          sprintf "Bestätigen Sie Ihre Zahlung von %s an %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Ihr Anmeldecode lautet:"
        | Passcode_reset -> "Mit diesem Code setzen Sie Ihren Passcode zurück:"
        | Payment { amount; payee } ->
          sprintf "Code zur Bestätigung einer Zahlung von %s an %s:" amount payee)
  ; expiry =
      (fun n ->
        sprintf "Der Code ist %d %s gültig." n (plural n ~one:"Minute" ~many:"Minuten"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in ->
          "Falls Sie sich nicht anmelden wollten, können Sie diese E-Mail ignorieren."
        | Passcode_reset ->
          "Falls Sie das Zurücksetzen nicht angefordert haben, verwenden Sie den Code \
           nicht und kontaktieren Sie uns."
        | Payment _ ->
          "Geben Sie ihn nur ein, wenn Sie diese Zahlung selbst veranlasst haben.")
  ; never_share =
      (fun p ->
        sprintf
          "Geben Sie diesen Code niemals weiter. Mitarbeitende von %s werden Sie nie \
           danach fragen."
          p.service_name)
  }
;;

let fr =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "Votre code de connexion %s" p.service_name
        | Passcode_reset ->
          sprintf "Votre code de réinitialisation du code d'accès %s" p.service_name
        | Payment { amount; payee } ->
          sprintf "Confirmez votre paiement de %s à %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Votre code de connexion :"
        | Passcode_reset -> "Utilisez ce code pour réinitialiser votre code d'accès :"
        | Payment { amount; payee } ->
          sprintf "Code de confirmation du paiement de %s à %s :" amount payee)
  ; expiry =
      (fun n ->
        sprintf "Il expire dans %d %s." n (plural n ~one:"minute" ~many:"minutes"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in ->
          "Si vous n'avez pas essayé de vous connecter, vous pouvez ignorer cet e-mail."
        | Passcode_reset ->
          "Si vous n'êtes pas à l'origine de cette demande, n'utilisez pas ce code et \
           contactez-nous."
        | Payment _ -> "Ne le saisissez que si vous êtes à l'origine de ce paiement.")
  ; never_share =
      (fun p ->
        sprintf
          "Ne communiquez jamais ce code. Le personnel de %s ne vous le demandera jamais."
          p.service_name)
  }
;;

let es =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "Tu código de inicio de sesión de %s" p.service_name
        | Passcode_reset ->
          sprintf "Tu código para restablecer el código de acceso de %s" p.service_name
        | Payment { amount; payee } -> sprintf "Confirma tu pago de %s a %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Tu código de inicio de sesión es:"
        | Passcode_reset -> "Usa este código para restablecer tu código de acceso:"
        | Payment { amount; payee } ->
          sprintf "Código para confirmar un pago de %s a %s:" amount payee)
  ; expiry =
      (fun n -> sprintf "Caduca en %d %s." n (plural n ~one:"minuto" ~many:"minutos"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Si no has intentado iniciar sesión, puedes ignorar este correo."
        | Passcode_reset ->
          "Si no lo has solicitado, no uses el código y ponte en contacto con nosotros."
        | Payment _ -> "Introdúcelo solo si has realizado tú este pago.")
  ; never_share =
      (fun p ->
        sprintf
          "No compartas nunca este código. El personal de %s nunca te lo pedirá."
          p.service_name)
  }
;;

let it =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "Il tuo codice di accesso a %s" p.service_name
        | Passcode_reset ->
          sprintf "Il tuo codice per reimpostare il passcode di %s" p.service_name
        | Payment { amount; payee } ->
          sprintf "Conferma il pagamento di %s a %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Il tuo codice di accesso è:"
        | Passcode_reset -> "Usa questo codice per reimpostare il passcode:"
        | Payment { amount; payee } ->
          sprintf "Codice per confermare un pagamento di %s a %s:" amount payee)
  ; expiry =
      (fun n -> sprintf "Scade tra %d %s." n (plural n ~one:"minuto" ~many:"minuti"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Se non hai provato ad accedere, puoi ignorare questa email."
        | Passcode_reset -> "Se non l'hai richiesto tu, non usare il codice e contattaci."
        | Payment _ -> "Inseriscilo solo se hai disposto tu questo pagamento.")
  ; never_share =
      (fun p ->
        sprintf
          "Non condividere mai questo codice. Il personale di %s non te lo chiederà mai."
          p.service_name)
  }
;;

let pt =
  { heading =
      (fun p ->
        match p.purpose with
        | Sign_in -> sprintf "O seu código de início de sessão em %s" p.service_name
        | Passcode_reset ->
          sprintf "O seu código para redefinir o código de acesso em %s" p.service_name
        | Payment { amount; payee } ->
          sprintf "Confirme o seu pagamento de %s a %s" amount payee)
  ; intro =
      (fun p ->
        match p.purpose with
        | Sign_in -> "O seu código de início de sessão é:"
        | Passcode_reset -> "Utilize este código para redefinir o seu código de acesso:"
        | Payment { amount; payee } ->
          sprintf "Código para confirmar um pagamento de %s a %s:" amount payee)
  ; expiry =
      (fun n ->
        sprintf "Expira dentro de %d %s." n (plural n ~one:"minuto" ~many:"minutos"))
  ; ignore =
      (fun p ->
        match p.purpose with
        | Sign_in -> "Se não tentou iniciar sessão, pode ignorar este e-mail."
        | Passcode_reset -> "Se não fez este pedido, não utilize o código e contacte-nos."
        | Payment _ -> "Introduza-o apenas se foi o próprio a fazer este pagamento.")
  ; never_share =
      (fun p ->
        sprintf
          "Nunca partilhe este código. A equipa de %s nunca lho pedirá."
          p.service_name)
  }
;;

let render words p =
  let n = minutes p.expires_in in
  let esc = Template.Html.escape in
  { Template.Rendered.subject = words.heading p
  ; text =
      String.concat
        ~sep:"\n"
        [ words.intro p
        ; ""
        ; "    " ^ p.code
        ; ""
        ; words.expiry n
        ; words.ignore p
        ; ""
        ; words.never_share p
        ; ""
        ]
  ; html =
      Some
        (String.concat
           ~sep:"\n"
           [ "<p>" ^ esc (words.intro p) ^ "</p>"
           ; "<p style=\"font-size:28px;font-weight:bold;letter-spacing:6px\">"
             ^ esc p.code
             ^ "</p>"
           ; "<p>" ^ esc (words.expiry n) ^ " " ^ esc (words.ignore p) ^ "</p>"
           ; "<p style=\"color:#555\">" ^ esc (words.never_share p) ^ "</p>"
           ; ""
           ])
  }
;;

let samples =
  let base =
    { code = "482913"
    ; purpose = Sign_in
    ; expires_in = Time_ns.Span.of_min 10.
    ; service_name = "Example Bank"
    }
  in
  [ base
  ; { base with purpose = Passcode_reset; expires_in = Time_ns.Span.of_sec 45. }
  ; { base with
      purpose = Payment { amount = "EUR 1,250.00"; payee = "Müller & Söhne <GmbH>" }
    ; service_name = "B&B \"Bank\""
    }
  ]
;;

let template =
  Template.create
    ~name:"one-time-code"
    ~samples
    ~must_mention:(fun p ->
      p.code
      :: p.service_name
      ::
      (match p.purpose with
       | Payment { amount; payee } -> [ amount; payee ]
       | Sign_in | Passcode_reset -> []))
    [ Template.Locale.en, render en
    ; Template.Locale.de, render de
    ; Template.Locale.fr, render fr
    ; Template.Locale.es, render es
    ; Template.Locale.it, render it
    ; Template.Locale.pt, render pt
    ]
;;
