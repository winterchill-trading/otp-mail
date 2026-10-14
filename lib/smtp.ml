open! Core
open! Async

module Security = struct
  type t =
    | Plain
    | Starttls
    | Implicit_tls
  [@@deriving sexp, compare, equal]

  let of_string s =
    match String.lowercase (String.strip s) with
    | "plain" -> Ok Plain
    | "starttls" -> Ok Starttls
    | "tls" | "implicit_tls" | "implicit-tls" -> Ok Implicit_tls
    | _ -> Or_error.error_s [%message "unknown SMTP security (plain, starttls or tls)" s]
  ;;
end

module Credentials = struct
  type t =
    { username : string
    ; password : string
    }

  let sexp_of_t t = [%sexp { username : string = t.username; password = "<redacted>" }]
  let create ~username ~password = { username; password }
end

module Config = struct
  type t =
    { host : string
    ; port : int
    ; security : Security.t
    ; credentials : Credentials.t option
    ; helo_name : string
    ; ca_file : string option
    ; timeout : Time_ns.Span.t
    ; allow_plaintext_auth : bool
    ; message_id_domain : string
    ; dkim : Dkim.Config.t option
    }
  [@@deriving sexp_of]

  let create
    ?port
    ?(security = Security.Starttls)
    ?credentials
    ?(helo_name = "localhost")
    ?ca_file
    ?(timeout = Time_ns.Span.of_sec 30.)
    ?(allow_plaintext_auth = false)
    ?dkim
    ~host
    ~message_id_domain
    ()
    =
    let port =
      match port, security with
      | Some p, _ -> p
      | None, Plain -> 25
      | None, Starttls -> 587
      | None, Implicit_tls -> 465
    in
    { host
    ; port
    ; security
    ; credentials
    ; helo_name
    ; ca_file
    ; timeout
    ; allow_plaintext_auth
    ; message_id_domain
    ; dkim
    }
  ;;

  let of_env () =
    let open Or_error.Let_syntax in
    let get name =
      Option.filter (Sys.getenv name) ~f:(fun v -> not (String.is_empty v))
    in
    let%bind host =
      match get "OTP_MAIL_SMTP_HOST" with
      | Some h -> Ok h
      | None -> Or_error.error_s [%message "OTP_MAIL_SMTP_HOST is not set"]
    in
    let%bind port =
      match get "OTP_MAIL_SMTP_PORT" with
      | None -> Ok None
      | Some p ->
        Or_error.try_with (fun () -> Some (Int.of_string p))
        |> Or_error.tag ~tag:"OTP_MAIL_SMTP_PORT"
    in
    let%map security =
      match get "OTP_MAIL_SMTP_SECURITY" with
      | None -> Ok Security.Starttls
      | Some s -> Security.of_string s
    in
    let credentials =
      match get "OTP_MAIL_SMTP_USERNAME", get "OTP_MAIL_SMTP_PASSWORD" with
      | Some username, Some password -> Some (Credentials.create ~username ~password)
      | _ -> None
    in
    create
      ?port
      ~security
      ?credentials
      ?helo_name:(get "OTP_MAIL_SMTP_HELO")
      ?ca_file:(get "OTP_MAIL_SMTP_CA_FILE")
      ~host
      ~message_id_domain:(Option.value (get "OTP_MAIL_MESSAGE_ID_DOMAIN") ~default:host)
      ()
  ;;
end

module Reply = struct
  type t =
    { code : int
    ; lines : string list
    }
  [@@deriving sexp, compare, equal]

  let to_string t = sprintf "%d %s" t.code (String.concat ~sep:"; " t.lines)
end

type t =
  { config : Config.t
  ; time_source : Time_source.t
  ; rng : Rng.t
  ; trace : [ `Client | `Server ] -> string -> unit
  ; feed : Mailer.Event_feed.t
  ; idempotency : Mailer.Idempotency.t
  }

let create ?time_source ?rng ?(trace = fun _ _ -> ()) config =
  { config
  ; time_source =
      (match time_source with
       | None -> Time_source.wall_clock ()
       | Some ts -> Time_source.read_only ts)
  ; rng = Option.value_or_thunk rng ~default:Rng.system
  ; trace
  ; feed = Mailer.Event_feed.create ()
  ; idempotency = Mailer.Idempotency.create ()
  }
;;

let events t = Mailer.Event_feed.subscribe t.feed

let dot_stuff wire =
  let lines =
    String.split wire ~on:'\n'
    |> List.map ~f:(fun l -> String.chop_suffix_if_exists l ~suffix:"\r")
  in
  let lines =
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  String.concat
    (List.map lines ~f:(fun l ->
       (if String.is_prefix l ~prefix:"." then "." ^ l else l) ^ "\r\n"))
  ^ ".\r\n"
;;

let default_ca_file () =
  let candidates =
    Option.to_list (Sys.getenv "SSL_CERT_FILE")
    @ [ "/etc/ssl/certs/ca-certificates.crt"
      ; "/etc/ssl/cert.pem"
      ; "/opt/homebrew/etc/openssl@3/cert.pem"
      ; "/usr/local/etc/openssl@3/cert.pem"
      ]
  in
  Deferred.List.find candidates ~f:(fun path ->
    match%map Sys.file_exists path with
    | `Yes -> true
    | `No | `Unknown -> false)
;;

type session =
  { reader : Reader.t
  ; writer : Writer.t
  ; tls : bool
  ; extensions : string list
  }

let transport_error fmt =
  ksprintf (fun s -> Error (Mailer.Send_error.Transport (Error.of_string s))) fmt
;;

let error_of_reply (reply : Reply.t) : Mailer.Send_error.t =
  if reply.code >= 400 && reply.code < 500
  then Temporary { message = Reply.to_string reply }
  else if reply.code >= 500 && reply.code < 600
  then Rejected { code = reply.code; message = Reply.to_string reply }
  else Transport (Error.create_s [%message "unexpected SMTP reply" (reply : Reply.t)])
;;

let with_timeout t d ~what =
  match%map Clock_ns.with_timeout t.config.timeout d with
  | `Result r -> r
  | `Timeout -> transport_error "SMTP timeout while %s" what
;;

let read_reply t reader =
  let rec loop acc =
    match%bind Reader.read_line reader with
    | `Eof -> return (transport_error "SMTP server closed the connection")
    | `Ok line ->
      let line = String.chop_suffix_if_exists line ~suffix:"\r" in
      t.trace `Server line;
      if String.length line < 3
         || not (String.for_all (String.prefix line 3) ~f:Char.is_digit)
      then return (transport_error "malformed SMTP reply: %S" line)
      else (
        let code = Int.of_string (String.prefix line 3) in
        let text = String.drop_prefix line 4 in
        let acc = text :: acc in
        if String.length line > 3 && Char.equal line.[3] '-'
        then loop acc
        else return (Ok { Reply.code; lines = List.rev acc }))
  in
  with_timeout t (loop []) ~what:"reading a reply"
;;

let redact line =
  if String.is_prefix (String.uppercase line) ~prefix:"AUTH "
  then (
    match String.split line ~on:' ' with
    | auth :: mechanism :: _ :: _ -> auth ^ " " ^ mechanism ^ " <redacted>"
    | _ -> line)
  else line
;;

let send_line t s ?(secret = false) line =
  t.trace `Client (if secret then "<redacted>" else redact line);
  Writer.write s.writer line;
  Writer.write s.writer "\r\n"
;;

let command t s ?secret line ~ok =
  send_line t s ?secret line;
  match%map read_reply t s.reader with
  | Error e -> Error e
  | Ok reply when List.mem ok reply.code ~equal:Int.equal -> Ok reply
  | Ok reply -> Error (error_of_reply reply)
;;

let ehlo t s =
  match%map command t s ("EHLO " ^ t.config.helo_name) ~ok:[ 250 ] with
  | Error e -> Error e
  | Ok reply ->
    Ok { s with extensions = List.map (List.drop reply.lines 1) ~f:String.uppercase }
;;

let extension s name =
  List.find_map s.extensions ~f:(fun line ->
    match String.split line ~on:' ' with
    | keyword :: args when String.equal keyword name -> Some args
    | _ -> None)
;;

let tls_config t ~ca_file =
  let host = t.config.host in
  Async_ssl.Config.Client.create
    ~remote_hostname:(Some host)
    ~ca_file
    ~ca_path:None
    ~verify_modes:[ Async_ssl.Verify_mode.Verify_peer ]
    ~verify_callback:(fun connection ->
      (* OpenSSL checks the chain; the host name is checked here, since SNI alone does not
         verify it. *)
      return (Async_ssl.Ssl.Connection.check_peer_certificate_host connection host))
    ()
;;

let with_tls t ~reader ~writer ~f =
  let%bind ca_file =
    match t.config.ca_file with
    | Some f -> return (Some f)
    | None -> default_ca_file ()
  in
  match%map
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      Async_ssl.Tls.wrap_client_connection
        ~timeout:t.config.timeout
        (tls_config t ~ca_file)
        reader
        writer
        ~f:(fun _connection reader writer -> f reader writer))
  with
  | Ok r -> r
  | Error e -> Error (Mailer.Send_error.Transport (Error.tag e ~tag:"TLS"))
;;

let authenticate t s =
  match t.config.credentials with
  | None -> return (Ok ())
  | Some { username; password } ->
    if (not s.tls) && not t.config.allow_plaintext_auth
    then return (transport_error "refusing to send SMTP credentials without TLS")
    else (
      let mechanisms = Option.value (extension s "AUTH") ~default:[] in
      let b64 = Mime.Base64.encode ~line_length:0 in
      if List.mem mechanisms "PLAIN" ~equal:String.equal
      then (
        let token = b64 ("\000" ^ username ^ "\000" ^ password) in
        Deferred.Result.ignore_m (command t s ("AUTH PLAIN " ^ token) ~ok:[ 235 ]))
      else if List.mem mechanisms "LOGIN" ~equal:String.equal
      then
        let open Deferred.Result.Let_syntax in
        let%bind (_ : Reply.t) = command t s "AUTH LOGIN" ~ok:[ 334 ] in
        let%bind (_ : Reply.t) = command t s ~secret:true (b64 username) ~ok:[ 334 ] in
        let%map (_ : Reply.t) = command t s ~secret:true (b64 password) ~ok:[ 235 ] in
        ()
      else
        return
          (transport_error
             "the SMTP server offers no supported AUTH mechanism (PLAIN or LOGIN)"))
;;

let transaction t s ~from ~recipients wire =
  let open Deferred.Result.Let_syntax in
  let%bind (_ : Reply.t) =
    command t s ("MAIL FROM:<" ^ Address.to_string from ^ ">") ~ok:[ 250 ]
  in
  let%bind.Deferred rcpts =
    Deferred.List.map ~how:`Sequential recipients ~f:(fun r ->
      send_line t s ("RCPT TO:<" ^ Address.to_string r ^ ">");
      Deferred.Result.map (read_reply t s.reader) ~f:(fun reply -> r, reply))
  in
  let%bind rcpts = Deferred.return (Result.all rcpts) in
  let accepted, refused =
    List.partition_tf rcpts ~f:(fun (_, (reply : Reply.t)) ->
      reply.code = 250 || reply.code = 251)
  in
  let%bind () =
    match accepted, refused with
    | [], (_, reply) :: _ -> Deferred.return (Error (error_of_reply reply))
    | [], [] -> Deferred.return (transport_error "no recipients")
    | _ :: _, _ -> return ()
  in
  let%bind (_ : Reply.t) = command t s "DATA" ~ok:[ 354 ] in
  t.trace `Client "<message data>";
  Writer.write s.writer (dot_stuff wire);
  let%bind.Deferred reply = read_reply t s.reader in
  let%bind reply =
    Deferred.return
      (match reply with
       | Ok r when r.code = 250 -> Ok r
       | Ok r -> Error (error_of_reply r)
       | Error e -> Error e)
  in
  send_line t s "QUIT";
  (* A missing or odd reply to QUIT does not undo the delivery. *)
  let%bind.Deferred (_ : (Reply.t, Mailer.Send_error.t) Result.t) =
    read_reply t s.reader
  in
  return (reply, refused)
;;

let session t ~reader ~writer ~tls ~from ~recipients wire =
  let open Deferred.Result.Let_syntax in
  let s = { reader; writer; tls; extensions = [] } in
  let%bind greeting = read_reply t reader in
  let%bind () =
    if greeting.code = 220
    then return ()
    else Deferred.return (Error (error_of_reply greeting))
  in
  let%bind s = ehlo t s in
  let continue s =
    let%bind () = authenticate t s in
    transaction t s ~from ~recipients wire
  in
  match t.config.security, s.tls with
  | Starttls, false ->
    if Option.is_none (extension s "STARTTLS")
    then
      Deferred.return
        (transport_error "the SMTP server does not offer STARTTLS; refusing to continue")
    else (
      let%bind (_ : Reply.t) = command t s "STARTTLS" ~ok:[ 220 ] in
      let%bind.Deferred () = Writer.flushed writer in
      with_tls t ~reader ~writer ~f:(fun reader writer ->
        let s = { reader; writer; tls = true; extensions = [] } in
        let%bind s = ehlo t s in
        continue s))
  | _ -> continue s
;;

let send_wire t ~from ~recipients wire =
  let where =
    Tcp.Where_to_connect.of_host_and_port
      { Host_and_port.host = t.config.host; port = t.config.port }
  in
  match%bind
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      Tcp.with_connection
        ~timeout:(Time_ns.Span.to_span_float_round_nearest t.config.timeout)
        where
        (fun _socket reader writer ->
           match t.config.security with
           | Implicit_tls ->
             with_tls t ~reader ~writer ~f:(fun reader writer ->
               session t ~reader ~writer ~tls:true ~from ~recipients wire)
           | Plain | Starttls ->
             session t ~reader ~writer ~tls:false ~from ~recipients wire))
  with
  | Ok r -> return r
  | Error e -> return (Error (Mailer.Send_error.Transport e))
;;

let deliver t (message : Message.t) =
  let now = Time_source.now t.time_source in
  let message_id = Message_id.generate ~rng:t.rng ~domain:t.config.message_id_domain in
  let delivery_id =
    Mailer.Delivery_id.of_string (Message_id.local message_id) |> Or_error.ok_exn
  in
  let wire = Message.render message ~date:now ~message_id in
  let signed =
    match t.config.dkim with
    | None -> Ok wire
    | Some config -> Dkim.sign config ~now wire
  in
  match signed, Message.recipients message with
  | Error e, _ -> return (Error (Mailer.Send_error.Invalid_message e))
  | Ok _, [] ->
    return
      (Error
         (Mailer.Send_error.Invalid_message (Error.of_string "message has no recipients")))
  | Ok wire, recipients ->
    (match%map send_wire t ~from:message.from.address ~recipients wire with
     | Error e -> Error e
     | Ok (_, refused) ->
       let at = Time_source.now t.time_source in
       Mailer.Event_feed.publish
         t.feed
         { delivery_id; recipient = None; status = Sent; at };
       List.iter refused ~f:(fun (recipient, reply) ->
         Mailer.Event_feed.publish
           t.feed
           { delivery_id
           ; recipient = Some recipient
           ; status = Failed { reason = Reply.to_string reply }
           ; at
           });
       Ok delivery_id)
;;

let send t ?idempotency_key message =
  Mailer.Idempotency.run t.idempotency ~key:idempotency_key message ~send:(fun () ->
    deliver t message)
;;

let mailer t =
  Mailer.pack
    (module struct
      type nonrec t = t

      let send = send
      let events = events
    end)
    t
;;
