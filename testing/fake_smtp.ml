open! Core
open! Async

module Received = struct
  type t =
    { mail_from : string
    ; rcpt_to : string list
    ; data : string
    ; tls : bool
    ; authenticated_as : string option
    }
  [@@deriving sexp_of]
end

type options =
  { tls : (string * string) option
  ; implicit_tls : bool
  ; advertise_starttls : bool
  ; credentials : (string * string) option
  ; auth_mechanisms : string list
  ; recipient_reply : string -> (int * string) option
  ; data_reply : (int * string) option
  ; greeting : (int * string) option
  }

type t =
  { server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; received : Received.t Queue.t
  ; transcript : string Queue.t
  }

let port t = Tcp.Server.listening_on t.server
let received t = Queue.to_list t.received
let transcript t = Queue.to_list t.transcript
let close t = Tcp.Server.close t.server

let reply w code lines =
  let n = List.length lines in
  List.iteri lines ~f:(fun i line ->
    Writer.writef w "%d%c%s\r\n" code (if i = n - 1 then ' ' else '-') line)
;;

let b64_decode s = Result.ok (Otp_mail.Mime.Base64.decode s)

let extract_address arg =
  match String.lsplit2 arg ~on:'<', String.rsplit2 arg ~on:'>' with
  | Some (_, rest), Some _ -> Some (fst (String.rsplit2_exn rest ~on:'>'))
  | _ -> None
;;

let server_config (crt_file, key_file) =
  Async_ssl.Config.Server.create ~crt_file ~key_file ~ca_file:None ~ca_path:None ()
;;

type state =
  { mutable mail_from : string option
  ; mutable rcpt_to : string list
  ; mutable authenticated_as : string option
  }

let rec serve t options ~reader ~writer ~tls ~greet =
  let st = { mail_from = None; rcpt_to = []; authenticated_as = None } in
  let log line = Queue.enqueue t.transcript line in
  let read_line () =
    match%map Reader.read_line reader with
    | `Eof -> None
    | `Ok l -> Some (String.chop_suffix_if_exists l ~suffix:"\r")
  in
  let ehlo_lines () =
    [ "fake.smtp.invalid" ]
    @ (if Option.is_some options.tls && options.advertise_starttls && not tls
       then [ "STARTTLS" ]
       else [])
    @ (if Option.is_some options.credentials
       then [ "AUTH " ^ String.concat ~sep:" " options.auth_mechanisms ]
       else [])
    @ [ "SIZE 10485760" ]
  in
  let check_login user pass =
    match options.credentials with
    | Some (u, p) when String.equal u user && String.equal p pass -> true
    | _ -> false
  in
  let rec read_data acc =
    match%bind read_line () with
    | None -> return None
    | Some "." -> return (Some (String.concat (List.rev acc)))
    | Some l ->
      let l = if String.is_prefix l ~prefix:".." then String.drop_prefix l 1 else l in
      read_data ((l ^ "\r\n") :: acc)
  in
  let rec loop () =
    match%bind read_line () with
    | None -> return ()
    | Some line ->
      let verb, arg =
        match String.lsplit2 line ~on:' ' with
        | Some (v, a) -> String.uppercase v, a
        | None -> String.uppercase line, ""
      in
      log
        (if String.equal verb "AUTH"
         then "AUTH " ^ List.hd_exn (String.split arg ~on:' ') ^ " <redacted>"
         else line);
      (match verb with
       | "EHLO" ->
         reply writer 250 (ehlo_lines ());
         loop ()
       | "HELO" ->
         reply writer 250 [ "fake.smtp.invalid" ];
         loop ()
       | "STARTTLS" ->
         (match options.tls with
          | Some files when (not tls) && options.advertise_starttls ->
            reply writer 220 [ "ready to start TLS" ];
            let%bind () = Writer.flushed writer in
            Async_ssl.Tls.wrap_server_connection
              (server_config files)
              reader
              writer
              ~f:(fun _ reader writer ->
                serve t options ~reader ~writer ~tls:true ~greet:false)
          | _ ->
            reply writer 502 [ "STARTTLS not available" ];
            loop ())
       | "AUTH" ->
         let parts = String.split arg ~on:' ' in
         (match List.map parts ~f:String.uppercase, parts with
          | "PLAIN" :: _, [ _; token ] ->
            (match Option.map (b64_decode token) ~f:(String.split ~on:'\000') with
             | Some [ _; user; pass ] when check_login user pass ->
               st.authenticated_as <- Some user;
               reply writer 235 [ "authenticated" ]
             | _ -> reply writer 535 [ "authentication failed" ]);
            loop ()
          | [ "LOGIN" ], _ ->
            reply writer 334 [ "VXNlcm5hbWU6" ];
            let%bind user = read_line () in
            log "<redacted>";
            reply writer 334 [ "UGFzc3dvcmQ6" ];
            let%bind pass = read_line () in
            log "<redacted>";
            (match Option.bind user ~f:b64_decode, Option.bind pass ~f:b64_decode with
             | Some user, Some pass when check_login user pass ->
               st.authenticated_as <- Some user;
               reply writer 235 [ "authenticated" ]
             | _ -> reply writer 535 [ "authentication failed" ]);
            loop ()
          | _ ->
            reply writer 504 [ "mechanism not supported" ];
            loop ())
       | "MAIL" ->
         if Option.is_some options.credentials && Option.is_none st.authenticated_as
         then reply writer 530 [ "authentication required" ]
         else (
           match extract_address arg with
           | Some a ->
             st.mail_from <- Some a;
             st.rcpt_to <- [];
             reply writer 250 [ "sender ok" ]
           | None -> reply writer 501 [ "syntax error" ]);
         loop ()
       | "RCPT" ->
         (match st.mail_from, extract_address arg with
          | None, _ -> reply writer 503 [ "need MAIL first" ]
          | _, None -> reply writer 501 [ "syntax error" ]
          | Some _, Some a ->
            (match options.recipient_reply a with
             | Some (code, text) -> reply writer code [ text ]
             | None ->
               st.rcpt_to <- st.rcpt_to @ [ a ];
               reply writer 250 [ "recipient ok" ]));
         loop ()
       | "DATA" ->
         (match st.mail_from, st.rcpt_to with
          | Some mail_from, (_ :: _ as rcpt_to) ->
            reply writer 354 [ "end data with <CR><LF>.<CR><LF>" ];
            (match%bind read_data [] with
             | None -> return ()
             | Some data ->
               (match options.data_reply with
                | Some (code, text) -> reply writer code [ text ]
                | None ->
                  Queue.enqueue
                    t.received
                    { Received.mail_from
                    ; rcpt_to
                    ; data
                    ; tls
                    ; authenticated_as = st.authenticated_as
                    };
                  reply
                    writer
                    250
                    [ "queued as FAKE" ^ Int.to_string (Queue.length t.received) ]);
               st.mail_from <- None;
               st.rcpt_to <- [];
               loop ())
          | _ ->
            reply writer 503 [ "need MAIL and RCPT first" ];
            loop ())
       | "RSET" ->
         st.mail_from <- None;
         st.rcpt_to <- [];
         reply writer 250 [ "reset" ];
         loop ()
       | "NOOP" ->
         reply writer 250 [ "ok" ];
         loop ()
       | "QUIT" ->
         reply writer 221 [ "bye" ];
         Writer.flushed writer
       | _ ->
         reply writer 500 [ "unknown command" ];
         loop ())
  in
  if greet
  then (
    match options.greeting with
    | Some (code, text) ->
      reply writer code [ text ];
      Writer.flushed writer
    | None ->
      reply writer 220 [ "fake.smtp.invalid ESMTP ready" ];
      loop ())
  else loop ()
;;

let start
  ?tls
  ?(implicit_tls = false)
  ?advertise_starttls
  ?credentials
  ?(auth_mechanisms = [ "PLAIN"; "LOGIN" ])
  ?(recipient_reply = fun _ -> None)
  ?data_reply
  ?greeting
  ()
  =
  let options =
    { tls
    ; implicit_tls
    ; advertise_starttls = Option.value advertise_starttls ~default:(Option.is_some tls)
    ; credentials
    ; auth_mechanisms
    ; recipient_reply
    ; data_reply
    ; greeting
    }
  in
  let received = Queue.create () in
  let transcript = Queue.create () in
  let t_ref = ref None in
  let%map server =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
      (fun _address reader writer ->
         let t = Option.value_exn !t_ref in
         match options.implicit_tls, options.tls with
         | true, Some files ->
           Async_ssl.Tls.wrap_server_connection
             (server_config files)
             reader
             writer
             ~f:(fun _ reader writer ->
               serve t options ~reader ~writer ~tls:true ~greet:true)
         | _ -> serve t options ~reader ~writer ~tls:false ~greet:true)
  in
  let t = { server; received; transcript } in
  t_ref := Some t;
  t
;;
