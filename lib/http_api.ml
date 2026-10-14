open! Core
open! Async

module Endpoint = struct
  type t =
    { tls : bool
    ; host : string
    ; port : int option
    ; target : string (** path and query, starting with [/] *)
    }
  [@@deriving sexp_of, compare, equal]

  let loopback = [ "localhost"; "127.0.0.1"; "[::1]" ]

  let of_string url =
    let error msg = Or_error.error_s [%message msg url] in
    let scheme_rest =
      match String.chop_prefix url ~prefix:"https://" with
      | Some rest -> Some (true, rest)
      | None ->
        Option.map (String.chop_prefix url ~prefix:"http://") ~f:(fun r -> false, r)
    in
    match scheme_rest with
    | None -> error "endpoint must start with https:// (or http:// for loopback)"
    | Some (tls, rest) ->
      let authority, target =
        match String.index rest '/' with
        | None -> rest, "/"
        | Some i -> String.prefix rest i, String.drop_prefix rest i
      in
      let authority_has_user_info = String.mem authority '@' in
      let host, port =
        if authority_has_user_info
        then authority, None
        else if String.is_prefix authority ~prefix:"["
        then (
          match String.lsplit2 authority ~on:']' with
          | Some (h, "") -> h ^ "]", None
          | Some (h, p) -> h ^ "]", Some (String.chop_prefix_if_exists p ~prefix:":")
          | None -> authority, None)
        else (
          match String.rsplit2 authority ~on:':' with
          | Some (h, p) -> h, Some p
          | None -> authority, None)
      in
      let port =
        match port with
        | None -> Ok None
        | Some p ->
          (match Int.of_string_opt p with
           | Some n when n >= 1 && n <= 65535 && String.for_all p ~f:Char.is_digit ->
             Ok (Some n)
           | _ -> error "invalid port in endpoint")
      in
      let bad_char c =
        Char.is_whitespace c || Char.to_int c < 0x21 || Char.to_int c > 0x7e
      in
      (match port with
       | Error e -> Error e
       | Ok port ->
         if authority_has_user_info
         then error "endpoint must not carry user info"
         else if String.is_empty host
         then error "endpoint has no host"
         else if String.mem target '#'
         then error "endpoint must not carry a fragment"
         else if String.exists url ~f:bad_char
         then error "endpoint holds spaces or control characters"
         else if (not tls) && not (List.mem loopback host ~equal:String.equal)
         then error "plain http is only allowed for loopback hosts"
         else Ok { tls; host; port; target })
  ;;

  let host t = t.host
  let port t = Option.value t.port ~default:(if t.tls then 443 else 80)
  let is_tls t = t.tls

  let authority t =
    match t.port with
    | None -> t.host
    | Some p -> sprintf "%s:%d" t.host p
  ;;

  let to_string t = (if t.tls then "https://" else "http://") ^ authority t ^ t.target

  (* The name to connect to and to check the certificate against: without IPv6 brackets. *)
  let connect_host t =
    String.chop_prefix_if_exists t.host ~prefix:"["
    |> String.chop_suffix_if_exists ~suffix:"]"
  ;;
end

module Token = struct
  type t = string

  let sexp_of_t _ = Sexp.Atom "<redacted>"

  let of_string s =
    if (not (String.is_empty s))
       && String.for_all s ~f:(fun c -> Char.to_int c >= 0x21 && Char.to_int c <= 0x7e)
    then Ok s
    else Or_error.error_s [%message "API token must be non-empty visible ASCII"]
  ;;

  let of_env ?(var = "OTP_MAIL_HTTP_API_TOKEN") () =
    match Sys.getenv var with
    | None | Some "" -> Or_error.error_s [%message "environment variable is not set" var]
    | Some s -> of_string (String.strip s) |> Or_error.tag ~tag:var
  ;;
end

module Shape = struct
  type t =
    { encode : Message.t -> message_id:Message_id.t -> wire:string -> Yojson.Safe.t
    ; delivery_id : Yojson.Safe.t -> string option
    ; error_message : Yojson.Safe.t -> string option
    }

  let member name : Yojson.Safe.t -> Yojson.Safe.t option = function
    | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
    | _ -> None
  ;;

  let id_reader id_field json =
    match member id_field json with
    | Some (`String s) when not (String.is_empty s) -> Some s
    | Some (`Int n) -> Some (Int.to_string n)
    | _ -> None
  ;;

  let error_message json =
    match member "message" json, member "error" json with
    | Some (`String s), _ -> Some s
    | _, Some (`String s) -> Some s
    | _, Some error ->
      (match member "message" error with
       | Some (`String s) -> Some s
       | _ -> None)
    | _ -> None
  ;;

  let mailbox (m : Mailbox.t) : Yojson.Safe.t =
    `Assoc
      (("email", `String (Address.to_string m.address))
       :: Option.value_map m.display_name ~default:[] ~f:(fun n -> [ "name", `String n ])
      )
  ;;

  let fields ?(id_field = "id") () =
    let encode (m : Message.t) ~message_id ~wire:_ : Yojson.Safe.t =
      let list name = function
        | [] -> []
        | l -> [ name, `List (List.map l ~f:mailbox) ]
      in
      `Assoc
        (List.concat
           [ [ "from", mailbox m.from ]
           ; list "to" m.to_
           ; list "cc" m.cc
           ; list "bcc" m.bcc
           ; Option.value_map m.reply_to ~default:[] ~f:(fun r ->
               [ "reply_to", mailbox r ])
           ; [ "subject", `String m.subject; "text", `String m.text ]
           ; Option.value_map m.html ~default:[] ~f:(fun h -> [ "html", `String h ])
           ; [ ( "headers"
               , `Assoc
                   (("Message-ID", `String (Message_id.to_string message_id))
                    :: List.map m.headers ~f:(fun (n, v) -> n, `String v)) )
             ]
           ])
    in
    { encode; delivery_id = id_reader id_field; error_message }
  ;;

  let raw_mime ?(field = "raw") ?(id_field = "id") () =
    let encode (m : Message.t) ~message_id:_ ~wire : Yojson.Safe.t =
      `Assoc
        [ field, `String (Mime.Base64.encode ~line_length:0 wire)
        ; ( "to"
          , `List
              (List.map (Message.recipients m) ~f:(fun a -> `String (Address.to_string a)))
          )
        ]
    in
    { encode; delivery_id = id_reader id_field; error_message }
  ;;
end

let valid_header_name name =
  (not (String.is_empty name))
  && String.for_all name ~f:(fun c ->
    Char.is_alphanum c || String.mem "!#$%&'*+-.^_`|~" c)
;;

module Config = struct
  type t =
    { endpoint : Endpoint.t
    ; token : Token.t option
    ; timeout : Time_ns.Span.t
    ; ca_file : string option
    ; idempotency_header : string option
    ; headers : (string * string) list
    ; message_id_domain : string
    ; dkim : Dkim.Config.t option
    ; max_response_bytes : int
    }
  [@@deriving sexp_of]

  let create
    ?token
    ?(timeout = Time_ns.Span.of_sec 30.)
    ?ca_file
    ?(idempotency_header = Some "Idempotency-Key")
    ?(headers = [])
    ?dkim
    ?(max_response_bytes = 1 lsl 20)
    ~endpoint
    ~message_id_domain
    ()
    =
    List.iter
      (Option.value_map idempotency_header ~default:[] ~f:(fun h -> [ h, "" ]) @ headers)
      ~f:(fun (name, value) ->
        if not (valid_header_name name)
        then raise_s [%message "Http_api.Config: invalid header name" name];
        if String.exists value ~f:(fun c -> Char.equal c '\r' || Char.equal c '\n')
        then raise_s [%message "Http_api.Config: line break in header value" name]);
    { endpoint
    ; token
    ; timeout
    ; ca_file
    ; idempotency_header
    ; headers
    ; message_id_domain
    ; dkim
    ; max_response_bytes
    }
  ;;

  let of_env () =
    let open Or_error.Let_syntax in
    let get name =
      Option.filter (Sys.getenv name) ~f:(fun v -> not (String.is_empty v))
    in
    let%bind endpoint =
      match get "OTP_MAIL_HTTP_API_URL" with
      | None -> Or_error.error_s [%message "OTP_MAIL_HTTP_API_URL is not set"]
      | Some url -> Endpoint.of_string url
    in
    let%bind token =
      match get "OTP_MAIL_HTTP_API_TOKEN" with
      | None -> Ok None
      | Some _ -> Token.of_env () >>| Option.some
    in
    let%map timeout =
      match get "OTP_MAIL_HTTP_API_TIMEOUT_SECONDS" with
      | None -> Ok None
      | Some s ->
        (match Float.of_string_opt s with
         | Some f when Float.( > ) f 0. -> Ok (Some (Time_ns.Span.of_sec f))
         | _ ->
           Or_error.error_s
             [%message "OTP_MAIL_HTTP_API_TIMEOUT_SECONDS is not a positive number"])
    in
    create
      ?token
      ?timeout
      ?ca_file:(get "OTP_MAIL_HTTP_API_CA_FILE")
      ~endpoint
      ~message_id_domain:
        (Option.value
           (get "OTP_MAIL_MESSAGE_ID_DOMAIN")
           ~default:(Endpoint.connect_host endpoint))
      ()
  ;;
end

module Response = struct
  type t =
    { status : int
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp_of]
end

let dechunk body =
  let rec go pos acc =
    match String.substr_index body ~pos ~pattern:"\r\n" with
    | None -> Or_error.error_s [%message "truncated chunked body"]
    | Some eol ->
      let size_text =
        String.sub body ~pos ~len:(eol - pos)
        |> String.lsplit2 ~on:';'
        |> Option.value_map ~default:(String.sub body ~pos ~len:(eol - pos)) ~f:fst
        |> String.strip
      in
      (match Int.of_string_opt ("0x" ^ size_text) with
       | None -> Or_error.error_s [%message "bad chunk size" size_text]
       | Some 0 -> Ok (String.concat (List.rev acc))
       | Some n when n < 0 || eol + 2 + n > String.length body ->
         Or_error.error_s [%message "truncated chunked body"]
       | Some n ->
         let chunk = String.sub body ~pos:(eol + 2) ~len:n in
         go (eol + 2 + n + 2) (chunk :: acc))
  in
  go 0 []
;;

let parse_response bytes =
  let open Or_error.Let_syntax in
  let%bind head, rest =
    match String.substr_index bytes ~pattern:"\r\n\r\n" with
    | Some i -> Ok (String.prefix bytes i, String.drop_prefix bytes (i + 4))
    | None -> Or_error.error_s [%message "HTTP answer without a complete header"]
  in
  let%bind status_line, header_lines =
    match
      String.split head ~on:'\n' |> List.map ~f:(String.rstrip ~drop:(Char.equal '\r'))
    with
    | [] -> Or_error.error_s [%message "empty HTTP answer"]
    | first :: rest -> Ok (first, rest)
  in
  let%bind status =
    match String.split status_line ~on:' ' with
    | version :: code :: _
      when String.is_prefix version ~prefix:"HTTP/1."
           && String.length code = 3
           && String.for_all code ~f:Char.is_digit -> Ok (Int.of_string code)
    | _ -> Or_error.error_s [%message "bad HTTP status line" status_line]
  in
  let headers =
    List.filter_map header_lines ~f:(fun line ->
      Option.map (String.lsplit2 line ~on:':') ~f:(fun (n, v) ->
        String.lowercase (String.strip n), String.strip v))
  in
  let find name = List.Assoc.find headers name ~equal:String.equal in
  let%map body =
    match find "transfer-encoding", find "content-length" with
    | Some te, _ when String.is_substring (String.lowercase te) ~substring:"chunked" ->
      dechunk rest
    | _, Some len ->
      (match Int.of_string_opt len with
       | Some n when n >= 0 && n <= String.length rest -> Ok (String.prefix rest n)
       | Some _ -> Or_error.error_s [%message "HTTP body shorter than Content-Length"]
       | None -> Or_error.error_s [%message "bad Content-Length" len])
    | _ -> Ok rest
  in
  { Response.status; headers; body }
;;

type t =
  { config : Config.t
  ; shape : Shape.t
  ; time_source : Time_source.t
  ; rng : Rng.t
  ; trace : string -> unit
  ; feed : Mailer.Event_feed.t
  ; idempotency : Mailer.Idempotency.t
  }

let create
  ?time_source
  ?(rng = Rng.system ())
  ?(shape = Shape.fields ())
  ?(trace = ignore)
  config
  =
  { config
  ; shape
  ; time_source =
      (match time_source with
       | None -> Time_source.wall_clock ()
       | Some ts -> Time_source.read_only ts)
  ; rng
  ; trace
  ; feed = Mailer.Event_feed.create ()
  ; idempotency = Mailer.Idempotency.create ()
  }
;;

let events t = Mailer.Event_feed.subscribe t.feed

let transport fmt =
  ksprintf (fun s -> Error (Mailer.Send_error.Transport (Error.of_string s))) fmt
;;

(* The same search as [Smtp]'s: an explicit file, then the usual system bundles. *)
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

let with_tls t ~reader ~writer ~f =
  let host = Endpoint.connect_host t.config.endpoint in
  let%bind ca_file =
    match t.config.ca_file with
    | Some f -> return (Some f)
    | None -> default_ca_file ()
  in
  let tls_config =
    Async_ssl.Config.Client.create
      ~remote_hostname:(Some host)
      ~ca_file
      ~ca_path:None
      ~verify_modes:[ Async_ssl.Verify_mode.Verify_peer ]
      ~verify_callback:(fun connection ->
        (* OpenSSL checks the chain; the host name is checked here. *)
        return (Async_ssl.Ssl.Connection.check_peer_certificate_host connection host))
      ()
  in
  Async_ssl.Tls.wrap_client_connection
    ~timeout:t.config.timeout
    tls_config
    reader
    writer
    ~f:(fun _connection reader writer -> f reader writer)
;;

let request_bytes t ~idempotency_key ~body =
  let endpoint = t.config.endpoint in
  let headers =
    List.concat
      [ [ "Host", Endpoint.authority endpoint
        ; "Content-Type", "application/json"
        ; "Accept", "application/json"
        ; "Content-Length", Int.to_string (String.length body)
        ; "User-Agent", "otp-mail"
        ; "Connection", "close"
        ]
      ; Option.value_map t.config.token ~default:[] ~f:(fun token ->
          [ "Authorization", "Bearer " ^ token ])
      ; (match t.config.idempotency_header, idempotency_key with
         | Some name, Some key -> [ name, key ]
         | _ -> [])
      ; t.config.headers
      ]
  in
  String.concat
    (("POST " ^ endpoint.target ^ " HTTP/1.1\r\n")
     :: List.map headers ~f:(fun (n, v) -> n ^ ": " ^ v ^ "\r\n"))
  ^ "\r\n"
  ^ body
;;

let exchange t request =
  let endpoint = t.config.endpoint in
  let talk reader writer =
    Writer.write writer request;
    let%bind () = Writer.flushed writer in
    let buffer = Buffer.create 4096 in
    let chunk = Bytes.create 4096 in
    let rec read () =
      match%bind Reader.read reader chunk with
      | `Eof -> return (Ok (Buffer.contents buffer))
      | `Ok n ->
        Buffer.add_subbytes buffer chunk ~pos:0 ~len:n;
        if Buffer.length buffer > t.config.max_response_bytes
        then
          return
            (transport "HTTP answer longer than %d bytes" t.config.max_response_bytes)
        else read ()
    in
    read ()
  in
  let where =
    Tcp.Where_to_connect.of_host_and_port
      { Host_and_port.host = Endpoint.connect_host endpoint
      ; port = Endpoint.port endpoint
      }
  in
  let run () =
    Tcp.with_connection
      ~timeout:(Time_ns.Span.to_span_float_round_nearest t.config.timeout)
      where
      (fun _socket reader writer ->
         if Endpoint.is_tls endpoint
         then with_tls t ~reader ~writer ~f:talk
         else talk reader writer)
  in
  match%map
    Clock_ns.with_timeout
      t.config.timeout
      (Monitor.try_with_or_error ~extract_exn:true run)
  with
  | `Timeout ->
    transport "HTTP timeout after %s" (Time_ns.Span.to_string_hum t.config.timeout)
  | `Result (Error e) -> Error (Mailer.Send_error.Transport e)
  | `Result (Ok (Error e)) -> Error e
  | `Result (Ok (Ok bytes)) ->
    (match parse_response bytes with
     | Ok r -> Ok r
     | Error e -> Error (Mailer.Send_error.Transport e))
;;

let shorten s = if String.length s <= 200 then s else String.prefix s 200 ^ "..."
let json_of_body body = Option.try_with (fun () -> Yojson.Safe.from_string body)

let outcome t (r : Response.t) ~fallback_id
  : (Mailer.Delivery_id.t, Mailer.Send_error.t) Result.t
  =
  let json = json_of_body r.body in
  let message () =
    Option.bind json ~f:t.shape.error_message
    |> Option.value ~default:(shorten (String.strip r.body))
  in
  if r.status >= 200 && r.status < 300
  then (
    match Option.bind json ~f:t.shape.delivery_id with
    | None -> Ok fallback_id
    | Some id ->
      (* The service accepted the message; an id we cannot use must not turn that into an
         error a caller would retry, so the generated one stands in. *)
      (match Mailer.Delivery_id.of_string id with
       | Ok id -> Ok id
       | Error _ -> Ok fallback_id))
  else if r.status = 408 || r.status = 429 || (r.status >= 500 && r.status < 600)
  then Error (Temporary { message = sprintf "HTTP %d: %s" r.status (message ()) })
  else if r.status >= 400 && r.status < 500
  then Error (Rejected { code = r.status; message = message () })
  else transport "unexpected HTTP status %d" r.status
;;

let deliver t ~idempotency_key (message : Message.t) =
  let now = Time_source.now t.time_source in
  let message_id = Message_id.generate ~rng:t.rng ~domain:t.config.message_id_domain in
  let fallback_id =
    Mailer.Delivery_id.of_string (Message_id.local message_id) |> Or_error.ok_exn
  in
  let wire = Message.render message ~date:now ~message_id in
  let signed =
    match t.config.dkim with
    | None -> Ok wire
    | Some config -> Dkim.sign config ~now wire
  in
  match signed with
  | Error e -> return (Error (Mailer.Send_error.Invalid_message e))
  | Ok wire ->
    (match
       Or_error.try_with (fun () ->
         Yojson.Safe.to_string (t.shape.encode message ~message_id ~wire))
     with
     | Error e ->
       return (Error (Mailer.Send_error.Invalid_message (Error.tag e ~tag:"encoding")))
     | Ok body ->
       t.trace ("POST " ^ Endpoint.to_string t.config.endpoint);
       let%map result = exchange t (request_bytes t ~idempotency_key ~body) in
       let result =
         match result with
         | Error e -> Error e
         | Ok r ->
           t.trace (sprintf "HTTP %d" r.status);
           outcome t r ~fallback_id
       in
       let at = Time_source.now t.time_source in
       (match result with
        | Ok delivery_id ->
          t.trace ("accepted as " ^ Mailer.Delivery_id.to_string delivery_id);
          Mailer.Event_feed.publish
            t.feed
            { delivery_id; recipient = None; status = Sent; at }
        | Error (Rejected { code; message }) ->
          t.trace (sprintf "rejected (%d)" code);
          Mailer.Event_feed.publish
            t.feed
            { delivery_id = fallback_id
            ; recipient = None
            ; status = Failed { reason = message }
            ; at
            }
        | Error e -> t.trace (Sexp.to_string [%sexp (e : Mailer.Send_error.t)]));
       result)
;;

let send t ?idempotency_key message =
  Mailer.Idempotency.run t.idempotency ~key:idempotency_key message ~send:(fun () ->
    deliver t ~idempotency_key message)
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
