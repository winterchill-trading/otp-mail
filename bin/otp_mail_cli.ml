(* Offline tools: nothing here sends email. *)

open! Core
open Otp_mail

let address_command =
  Command.basic
    ~summary:"check an email address (RFC 5322 addr-spec)"
    (let%map_open.Command address = anon ("ADDRESS" %: string) in
     fun () ->
       match Address.of_string address with
       | Ok a ->
         printf
           "local part: %s\ndomain:     %s\n"
           (Address.local_part a)
           (Address.domain a)
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let purpose_arg =
  Command.Arg_type.of_alist_exn
    [ "sign-in", `Sign_in; "passcode-reset", `Passcode_reset; "payment", `Payment ]
;;

let render_command =
  Command.basic
    ~summary:"render the one-time code email as it would be sent"
    ~readme:(fun () ->
      "Prints the RFC 5322 message (CRLF line breaks) to stdout. Without -code a random \
       code is drawn. With -dkim-key the message is signed.")
    (let%map_open.Command locale =
       flag "locale" (optional_with_default "en" string) ~doc:"TAG en, de, fr, es, it, pt"
     and purpose =
       flag
         "purpose"
         (optional_with_default `Sign_in purpose_arg)
         ~doc:"P sign-in, passcode-reset or payment"
     and amount =
       flag "amount" (optional_with_default "EUR 25.00" string) ~doc:"TEXT payment amount"
     and payee =
       flag "payee" (optional_with_default "Acme Ltd" string) ~doc:"TEXT payment payee"
     and code = flag "code" (optional string) ~doc:"DIGITS the code to show"
     and service =
       flag
         "service"
         (optional_with_default "Example Bank" string)
         ~doc:"NAME sender name"
     and from =
       flag
         "from"
         (optional_with_default "no-reply@bank.example" string)
         ~doc:"ADDRESS sender"
     and to_ =
       flag
         "to"
         (optional_with_default "customer@example.org" string)
         ~doc:"ADDRESS recipient"
     and dkim_key = flag "dkim-key" (optional string) ~doc:"PEM RSA key to sign with"
     and dkim_selector =
       flag "dkim-selector" (optional_with_default "mail" string) ~doc:"S selector"
     and seed =
       flag
         "seed"
         (optional string)
         ~doc:"SEED fixed random seed (16+ bytes), for reproducible output"
     and date = flag "date" (optional string) ~doc:"TIME Date header (default: now)" in
     fun () ->
       let open Or_error.Let_syntax in
       let result =
         let rng = Option.value_map seed ~f:Rng.of_seed ~default:(Rng.system ()) in
         let%bind locale = Template.Locale.of_string locale in
         let%bind from = Address.of_string from in
         let%bind to_ = Address.of_string to_ in
         let code = Option.value_or_thunk code ~default:(fun () -> Rng.digits rng 6) in
         let purpose : Code_email.purpose =
           match purpose with
           | `Sign_in -> Sign_in
           | `Passcode_reset -> Passcode_reset
           | `Payment -> Payment { amount; payee }
         in
         let%bind message =
           Template.to_message
             Code_email.template
             ~locale
             { code
             ; purpose
             ; expires_in = Time_ns.Span.of_min 10.
             ; service_name = service
             }
             ~from:(Mailbox.create ~display_name:service from)
             ~to_:[ Mailbox.of_address to_ ]
             ()
         in
         let now =
           Option.value_map
             date
             ~f:Time_ns.of_string_with_utc_offset
             ~default:(Time_ns.now ())
         in
         let wire =
           Message.render
             message
             ~date:now
             ~message_id:(Message_id.generate ~rng ~domain:(Address.domain from))
         in
         match dkim_key with
         | None -> Ok wire
         | Some path ->
           let%bind key = Dkim.Private_key.of_pem (In_channel.read_all path) in
           Dkim.sign
             (Dkim.Config.create
                ~domain:(Address.domain from)
                ~selector:dkim_selector
                key)
             ~now
             wire
       in
       match result with
       | Ok wire -> print_string wire
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let dkim_record_command =
  Command.basic
    ~summary:"print the DNS TXT record for a DKIM key"
    (let%map_open.Command key = flag "key" (required string) ~doc:"PEM RSA private key"
     and selector =
       flag "selector" (optional_with_default "mail" string) ~doc:"S selector"
     and domain =
       flag "domain" (optional_with_default "example.com" string) ~doc:"D signing domain"
     in
     fun () ->
       match Dkim.Private_key.of_pem (In_channel.read_all key) with
       | Ok key ->
         let record = Dkim.Public_key.to_dns_record (Dkim.Private_key.public_key key) in
         (* A TXT string holds at most 255 bytes; zone files join adjacent strings. *)
         let chunks =
           List.init
             ((String.length record + 249) / 250)
             ~f:(fun i ->
               String.sub
                 record
                 ~pos:(i * 250)
                 ~len:(min 250 (String.length record - (i * 250))))
         in
         printf
           "%s._domainkey.%s TXT %s\n"
           selector
           domain
           (String.concat ~sep:" " (List.map chunks ~f:(fun c -> "\"" ^ c ^ "\"")))
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let dkim_verify_command =
  Command.basic
    ~summary:"check a message's first DKIM signature against a key record"
    (let%map_open.Command record =
       flag "record" (required string) ~doc:"TXT the key record (v=DKIM1; k=rsa; p=...)"
     and file = anon ("MESSAGE" %: Filename_unix.arg_type) in
     fun () ->
       match
         Or_error.bind (Dkim.Public_key.of_dns_record record) ~f:(fun key ->
           Dkim.verify key (In_channel.read_all file))
       with
       | Ok () -> print_endline "signature verifies"
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let check_templates_command =
  Command.basic
    ~summary:"run the catalogue check over the built-in templates"
    (Command.Param.return (fun () ->
       match
         Template.Catalogue.check
           ~required_locales:Template.Locale.[ en; de; fr; es; it; pt ]
           [ T Code_email.template ]
       with
       | [] -> print_endline "no problems"
       | problems ->
         List.iter problems ~f:(fun p ->
           print_s [%sexp (p : Template.Catalogue.Problem.t)]);
         exit 1))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"one-time code email tools (offline; nothing is sent)"
       [ "address", address_command
       ; "render", render_command
       ; "dkim-record", dkim_record_command
       ; "dkim-verify", dkim_verify_command
       ; "check-templates", check_templates_command
       ])
;;
