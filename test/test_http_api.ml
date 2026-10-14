open! Core
open! Async
open Otp_mail

(* A loopback HTTP server that records each request and answers from a script. *)
module Fake_api = struct
  type request =
    { request_line : string
    ; headers : (string * string) list
    ; body : string
    }

  type t =
    { server : (Socket.Address.Inet.t, int) Tcp.Server.t
    ; requests : request Queue.t
    }

  let read_request reader =
    let rec headers acc =
      match%bind Reader.read_line reader with
      | `Eof | `Ok "" -> return (List.rev acc)
      | `Ok line ->
        (match String.lsplit2 line ~on:':' with
         | Some (n, v) -> headers ((String.lowercase n, String.strip v) :: acc)
         | None -> headers acc)
    in
    match%bind Reader.read_line reader with
    | `Eof -> return None
    | `Ok request_line ->
      let%bind headers = headers [] in
      let length =
        List.Assoc.find headers "content-length" ~equal:String.equal
        |> Option.value_map ~default:0 ~f:Int.of_string
      in
      let buf = Bytes.create length in
      let%map (_ : [ `Ok | `Eof of int ]) = Reader.really_read reader buf in
      Some { request_line; headers; body = Bytes.to_string buf }
  ;;

  (* [answer] returns the raw bytes to send back, or [None] to never answer. *)
  let start ~answer =
    let requests = Queue.create () in
    let%map server =
      Tcp.Server.create
        ~on_handler_error:`Ignore
        (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
        (fun _ reader writer ->
           match%bind read_request reader with
           | None -> return ()
           | Some request ->
             Queue.enqueue requests request;
             (match answer request with
              | None -> Deferred.never ()
              | Some bytes ->
                Writer.write writer bytes;
                Writer.flushed writer))
    in
    { server; requests }
  ;;

  let port t = Tcp.Server.listening_on t.server
  let close t = Tcp.Server.close t.server
end

let json_answer ?(status = "200 OK") body =
  sprintf
    "HTTP/1.1 %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
    status
    (String.length body)
    body
;;

let message () =
  Message.create
    ~from:(Helpers.mailbox ~name:"Example Bank" "no-reply@bank.example")
    ~to_:[ Helpers.mailbox ~name:"Ada" "ada@example.com" ]
    ~bcc:[ Helpers.mailbox "audit@bank.example" ]
    ~headers:[ "X-Entity-Ref-ID", "ref-1" ]
    ~html:"<p>482913</p>"
    ~subject:"Your sign-in code"
    ~text:"482913"
    ()
  |> Or_error.ok_exn
;;

let mailer ?shape ?(timeout = Time_ns.Span.of_sec 5.) fake =
  let config =
    Http_api.Config.create
      ~token:(Http_api.Token.of_string "test-token-not-a-secret" |> Or_error.ok_exn)
      ~timeout
      ~endpoint:
        (Http_api.Endpoint.of_string
           (sprintf "http://127.0.0.1:%d/v1/send?mode=transactional" (Fake_api.port fake))
         |> Or_error.ok_exn)
      ~message_id_domain:"bank.example"
      ()
  in
  Http_api.create
    ~time_source:(Time_source.create ~now:Helpers.fixed_date ())
    ~rng:(Rng.of_seed "http-api-test-seed-01")
    ?shape
    config
;;

let show_send r =
  print_s
    [%sexp
      (Result.map r ~f:Mailer.Delivery_id.to_string
       : (string, Mailer.Send_error.t) Result.t)]
;;

let%expect_test "endpoints" =
  List.iter
    [ "https://api.mail.example/v3/messages"
    ; "https://api.mail.example:8443"
    ; "http://localhost:8080/send"
    ; "http://[::1]:9000/send"
    ; "http://api.mail.example/send"
    ; "https://user:pw@api.mail.example/send"
    ; "https://api.mail.example/send#frag"
    ; "https://api.mail.example:99999/send"
    ; "ftp://api.mail.example"
    ]
    ~f:(fun url ->
      match Http_api.Endpoint.of_string url with
      | Ok e ->
        printf
          "%s -> %s %s:%d tls=%b\n"
          url
          (Http_api.Endpoint.to_string e)
          (Http_api.Endpoint.host e)
          (Http_api.Endpoint.port e)
          (Http_api.Endpoint.is_tls e)
      | Error e -> printf "%s -> %s\n" url (Error.to_string_hum e));
  [%expect
    {|
    https://api.mail.example/v3/messages -> https://api.mail.example/v3/messages api.mail.example:443 tls=true
    https://api.mail.example:8443 -> https://api.mail.example:8443/ api.mail.example:8443 tls=true
    http://localhost:8080/send -> http://localhost:8080/send localhost:8080 tls=false
    http://[::1]:9000/send -> http://[::1]:9000/send [::1]:9000 tls=false
    http://api.mail.example/send -> ("plain http is only allowed for loopback hosts"
     http://api.mail.example/send)
    https://user:pw@api.mail.example/send -> ("endpoint must not carry user info" https://user:pw@api.mail.example/send)
    https://api.mail.example/send#frag -> ("endpoint must not carry a fragment" https://api.mail.example/send#frag)
    https://api.mail.example:99999/send -> ("invalid port in endpoint" https://api.mail.example:99999/send)
    ftp://api.mail.example -> ("endpoint must start with https:// (or http:// for loopback)"
     ftp://api.mail.example)
    |}];
  return ()
;;

let%expect_test "parsing answers" =
  let show s =
    print_s [%sexp (Http_api.parse_response s : Http_api.Response.t Or_error.t)]
  in
  show "HTTP/1.1 201 Created\r\nContent-Length: 4\r\nX-A: b\r\n\r\nbodyEXTRA";
  show
    "HTTP/1.1 200 OK\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n\
     4\r\n\
     Wiki\r\n\
     5;x=1\r\n\
     pedia\r\n\
     0\r\n\
     \r\n";
  show "HTTP/1.0 202 Accepted\r\n\r\nuntil the end";
  show "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nshort";
  show "SMTP 220 hello\r\n\r\n";
  show "HTTP/1.1 200 OK\r\n";
  [%expect
    {|
    (Ok ((status 201) (headers ((content-length 4) (x-a b))) (body body)))
    (Ok ((status 200) (headers ((transfer-encoding chunked))) (body Wikipedia)))
    (Ok ((status 202) (headers ()) (body "until the end")))
    (Error "HTTP body shorter than Content-Length")
    (Error ("bad HTTP status line" "SMTP 220 hello"))
    (Error "HTTP answer without a complete header")
    |}];
  return ()
;;

let%expect_test "a message as fields, the request on the wire" =
  let%bind fake =
    Fake_api.start ~answer:(fun _ ->
      Some (json_answer ~status:"201 Created" {|{"id":"msg_0001","status":"queued"}|}))
  in
  let t = mailer fake in
  let events = Http_api.events t in
  let%bind r = Http_api.send t ~idempotency_key:"challenge-1" (message ()) in
  show_send r;
  let request = Queue.dequeue_exn fake.requests in
  print_endline request.request_line;
  List.iter request.headers ~f:(fun (n, v) ->
    if not (String.equal n "host") then printf "%s: %s\n" n v);
  print_endline (Yojson.Safe.pretty_to_string (Yojson.Safe.from_string request.body));
  let%bind event = Pipe.read events in
  (match event with
   | `Ok e ->
     print_s [%sexp (e.delivery_id : Mailer.Delivery_id.t), (e.status : Mailer.Status.t)]
   | `Eof -> print_endline "no event");
  (* The same key and message answer the first id without a second request. *)
  let%bind r = Http_api.send t ~idempotency_key:"challenge-1" (message ()) in
  show_send r;
  print_s [%sexp (Queue.length fake.requests : int)];
  let%bind () = Fake_api.close fake in
  [%expect
    {|
    (Ok msg_0001)
    POST /v1/send?mode=transactional HTTP/1.1
    content-type: application/json
    accept: application/json
    content-length: 321
    user-agent: otp-mail
    connection: close
    authorization: Bearer test-token-not-a-secret
    idempotency-key: challenge-1
    {
      "from": { "email": "no-reply@bank.example", "name": "Example Bank" },
      "to": [ { "email": "ada@example.com", "name": "Ada" } ],
      "bcc": [ { "email": "audit@bank.example" } ],
      "subject": "Your sign-in code",
      "text": "482913",
      "html": "<p>482913</p>",
      "headers": {
        "Message-ID": "<fc609b61fa4eeebc33e58dc2f4bc7bc3@bank.example>",
        "X-Entity-Ref-ID": "ref-1"
      }
    }
    (msg_0001 Sent)
    (Ok msg_0001)
    0
    |}];
  return ()
;;

let%expect_test "raw MIME shape" =
  let%bind fake = Fake_api.start ~answer:(fun _ -> Some (json_answer "{}")) in
  let t = mailer ~shape:(Http_api.Shape.raw_mime ~field:"mime" ()) fake in
  let%bind r = Http_api.send t (message ()) in
  (* No id in the answer: the generated Message-ID's left half. *)
  show_send r;
  let request = Queue.dequeue_exn fake.requests in
  let json = Yojson.Safe.from_string request.body in
  let wire =
    Yojson.Safe.Util.(member "mime" json |> to_string)
    |> Mime.Base64.decode
    |> Or_error.ok_exn
  in
  let parsed = Message.of_wire wire |> Or_error.ok_exn in
  print_s
    [%sexp
      (parsed.subject : string)
      , (Yojson.Safe.Util.member "to" json |> Yojson.Safe.to_string : string)
      , (String.is_substring wire ~substring:"audit@bank.example" : bool)];
  let%bind () = Fake_api.close fake in
  [%expect
    {|
    (Ok fc609b61fa4eeebc33e58dc2f4bc7bc3)
    ("Your sign-in code" "[\"ada@example.com\",\"audit@bank.example\"]" false)
    |}];
  return ()
;;

let%expect_test "errors" =
  let answers =
    Queue.of_list
      [ Some (json_answer ~status:"429 Too Many Requests" {|{"message":"slow down"}|})
      ; Some
          (json_answer
             ~status:"422 Unprocessable Entity"
             {|{"error":{"message":"recipient is suppressed"}}|})
      ; Some (json_answer ~status:"401 Unauthorized" {|{"error":"invalid token"}|})
      ; Some (json_answer ~status:"503 Service Unavailable" "<html>down</html>")
      ; Some (json_answer {|{"id":"bad id with spaces"}|})
      ; Some (json_answer {|{"id":12345}|})
      ; Some "garbage"
      ; None
      ]
  in
  let%bind fake = Fake_api.start ~answer:(fun _ -> Queue.dequeue_exn answers) in
  let t = mailer ~timeout:(Time_ns.Span.of_sec 0.5) fake in
  let%bind () =
    Deferred.List.iter ~how:`Sequential (List.range 0 8) ~f:(fun _ ->
      let%map r = Http_api.send t (message ()) in
      match r with
      | Ok id -> print_endline ("ok " ^ Mailer.Delivery_id.to_string id)
      | Error e ->
        printf
          "retryable=%b %s\n"
          (Mailer.Send_error.is_retryable e)
          (match e with
           | Transport _ -> "Transport"
           | other -> Sexp.to_string [%sexp (other : Mailer.Send_error.t)]))
  in
  let%bind () = Fake_api.close fake in
  [%expect
    {|
    retryable=true (Temporary(message"HTTP 429: slow down"))
    retryable=false (Rejected(code 422)(message"recipient is suppressed"))
    retryable=false (Rejected(code 401)(message"invalid token"))
    retryable=true (Temporary(message"HTTP 503: <html>down</html>"))
    ok 432323002d970c65366c6fb8a57fcda6
    ok 12345
    retryable=true Transport
    retryable=true Transport
    |}];
  return ()
;;

let%expect_test "tokens never print; configuration from the environment" =
  print_s [%sexp (Http_api.Token.of_string "s3cr3t" : Http_api.Token.t Or_error.t)];
  print_s [%sexp (Http_api.Token.of_string "has space" |> Result.is_error : bool)];
  let set k v = (Core_unix.putenv [@alert "-unsafe_multidomain"]) ~key:k ~data:v in
  set "OTP_MAIL_HTTP_API_URL" "https://api.mail.example/v1/send";
  set "OTP_MAIL_HTTP_API_TOKEN" "env-token";
  set "OTP_MAIL_HTTP_API_TIMEOUT_SECONDS" "12";
  let config = Http_api.Config.of_env () |> Or_error.ok_exn in
  print_s [%sexp (config : Http_api.Config.t)];
  set "OTP_MAIL_HTTP_API_URL" "http://api.mail.example/v1/send";
  print_s [%sexp (Http_api.Config.of_env () |> Result.is_error : bool)];
  [%expect
    {|
    (Ok <redacted>)
    true
    ((endpoint ((tls true) (host api.mail.example) (port ()) (target /v1/send)))
     (token (<redacted>)) (timeout 12s) (ca_file ())
     (idempotency_header (Idempotency-Key)) (headers ())
     (message_id_domain api.mail.example) (dkim ()) (max_response_bytes 1048576))
    true
    |}];
  return ()
;;
