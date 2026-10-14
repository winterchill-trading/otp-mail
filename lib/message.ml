open! Core

module Stable = struct
  module V1 = struct
    type t =
      { from : Mailbox.Stable.V1.t
      ; to_ : Mailbox.Stable.V1.t list
      ; cc : Mailbox.Stable.V1.t list
      ; bcc : Mailbox.Stable.V1.t list
      ; reply_to : Mailbox.Stable.V1.t option
      ; subject : string
      ; text : string
      ; html : string option
      ; headers : (string * string) list
      }
    [@@deriving sexp, bin_io, compare, equal, stable_witness]
  end
end

include Stable.V1

let reserved =
  [ "From"
  ; "To"
  ; "Cc"
  ; "Bcc"
  ; "Reply-To"
  ; "Subject"
  ; "Date"
  ; "Message-ID"
  ; "MIME-Version"
  ; "Content-Type"
  ; "Content-Transfer-Encoding"
  ; "DKIM-Signature"
  ]
;;

let is_reserved name = List.mem reserved name ~equal:String.Caseless.equal

let normalise_newlines s =
  String.substr_replace_all s ~pattern:"\r\n" ~with_:"\n"
  |> String.map ~f:(fun c -> if Char.equal c '\r' then '\n' else c)
;;

let make
  ~require_recipients
  ~from
  ?(to_ = [])
  ?(cc = [])
  ?(bcc = [])
  ?reply_to
  ?(headers = [])
  ?html
  ~subject
  ~text
  ()
  =
  let open Or_error.Let_syntax in
  let%bind () =
    if require_recipients && List.is_empty to_ && List.is_empty cc && List.is_empty bcc
    then Or_error.error_s [%message "message has no recipients"]
    else Ok ()
  in
  let%bind () =
    if String.exists subject ~f:(fun c -> Char.equal c '\r' || Char.equal c '\n')
    then Or_error.error_s [%message "subject contains a line break"]
    else Ok ()
  in
  let%map headers =
    List.map headers ~f:(fun (name, value) ->
      let value = String.strip value in
      if not (Mime.Header.valid_name name)
      then Or_error.error_s [%message "invalid header name" name]
      else if is_reserved name
      then Or_error.error_s [%message "header is written by the library" name]
      else if not
                (String.for_all value ~f:(fun c ->
                   let code = Char.to_int c in
                   (code >= 32 && code < 127) || Char.equal c '\t'))
      then Or_error.error_s [%message "header value is not printable ASCII" name]
      else Ok (name, value))
    |> Or_error.all
  in
  { from
  ; to_
  ; cc
  ; bcc
  ; reply_to
  ; subject = String.strip subject
  ; text = normalise_newlines text
  ; html = Option.map html ~f:normalise_newlines
  ; headers
  }
;;

let create = make ~require_recipients:true

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  match
    create
      ~from:t.from
      ~to_:t.to_
      ~cc:t.cc
      ~bcc:t.bcc
      ?reply_to:t.reply_to
      ~headers:t.headers
      ?html:t.html
      ~subject:t.subject
      ~text:t.text
      ()
  with
  | Ok t -> t
  | Error e -> of_sexp_error (Error.to_string_hum e) sexp
;;

let recipients t =
  List.map (t.to_ @ t.cc @ t.bcc) ~f:(fun (m : Mailbox.t) -> m.address)
  |> List.stable_dedup ~compare:Address.compare
;;

let rfc5322_date time =
  let date, ofday = Time_ns.to_date_ofday time ~zone:Time_float.Zone.utc in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf
    "%s, %d %s %d %02d:%02d:%02d +0000"
    (Day_of_week.to_string (Date.day_of_week date))
    (Date.day date)
    (Month.to_string (Date.month date))
    (Date.year date)
    parts.hr
    parts.min
    parts.sec
;;

(* 7bit needs short CRLF lines of printable ASCII; trailing whitespace goes through
   quoted-printable too, because some relays strip it. *)
let fits_7bit body ~boundary =
  List.for_all (String.split body ~on:'\n') ~f:(fun line ->
    String.length line <= 76
    && String.for_all line ~f:(fun c ->
      let code = Char.to_int c in
      (code >= 32 && code < 127) || Char.equal c '\t')
    && not (String.is_suffix line ~suffix:" " || String.is_suffix line ~suffix:"\t"))
  && not (String.is_substring body ~substring:boundary)
;;

let crlf s = String.substr_replace_all s ~pattern:"\n" ~with_:"\r\n"

let leaf ~subtype body ~boundary =
  let content_type =
    Mime.Content_type.create "text" subtype ~params:[ "charset", "utf-8" ]
  in
  if fits_7bit body ~boundary
  then
    ( [ "Content-Type", Mime.Content_type.to_string content_type
      ; "Content-Transfer-Encoding", "7bit"
      ]
    , crlf body )
  else
    ( [ "Content-Type", Mime.Content_type.to_string content_type
      ; "Content-Transfer-Encoding", "quoted-printable"
      ]
    , Mime.Quoted_printable.encode body )
;;

let boundary_for t =
  let digest =
    Cryptokit.hash_string
      (Cryptokit.Hash.sha256 ())
      (t.text ^ "\000" ^ Option.value t.html ~default:"")
  in
  (* "=_" cannot appear in quoted-printable output, and a 7bit part that contains the
     boundary is sent as quoted-printable instead (see [fits_7bit]). *)
  "=_" ^ String.prefix (Cryptokit.transform_string (Cryptokit.Hexa.encode ()) digest) 32
;;

let render t ~date ~message_id =
  let addresses name = function
    | [] -> []
    | mailboxes -> [ name, Mailbox.list_to_string mailboxes ]
  in
  let envelope =
    [ [ "Date", rfc5322_date date; "From", Mailbox.to_string t.from ]
    ; addresses "To" t.to_
    ; addresses "Cc" t.cc
    ; Option.to_list (Option.map t.reply_to ~f:(fun m -> "Reply-To", Mailbox.to_string m))
    ; [ "Subject", Mime.Encoded_word.encode t.subject
      ; "Message-ID", Message_id.to_string message_id
      ; "MIME-Version", "1.0"
      ]
    ; t.headers
    ]
    |> List.concat
  in
  let boundary = boundary_for t in
  match t.html with
  | None ->
    let headers, body = leaf ~subtype:"plain" t.text ~boundary in
    Mime.Header.to_string (envelope @ headers) ^ "\r\n" ^ body
  | Some html ->
    let part subtype body =
      let headers, body = leaf ~subtype body ~boundary in
      "--" ^ boundary ^ "\r\n" ^ Mime.Header.to_string headers ^ "\r\n" ^ body ^ "\r\n"
    in
    let content_type =
      Mime.Content_type.create "multipart" "alternative" ~params:[ "boundary", boundary ]
    in
    Mime.Header.to_string
      (envelope @ [ "Content-Type", Mime.Content_type.to_string content_type ])
    ^ "\r\n"
    ^ part "plain" t.text
    ^ part "html" html
    ^ "--"
    ^ boundary
    ^ "--\r\n"
;;

let of_wire s =
  let open Or_error.Let_syntax in
  let%bind entity = Mime.Entity.parse s in
  let headers = entity.headers in
  let mailboxes name =
    Mime.Header.find_all headers name
    |> List.map ~f:Mailbox.list_of_string
    |> Or_error.all
    >>| List.concat
  in
  let%bind from =
    match Mime.Header.find headers "From" with
    | None -> Or_error.error_s [%message "message has no From"]
    | Some v -> Mailbox.of_string v
  in
  let%bind to_ = mailboxes "To" in
  let%bind cc = mailboxes "Cc" in
  let%bind reply_to =
    match Mime.Header.find headers "Reply-To" with
    | None -> Ok None
    | Some v -> Mailbox.of_string v >>| Option.some
  in
  let subject =
    Mime.Header.find headers "Subject"
    |> Option.value ~default:""
    |> Mime.Encoded_word.decode
  in
  let content mime_type =
    Option.bind (Mime.Entity.find entity ~mime_type) ~f:Mime.Entity.leaf_content
  in
  let%bind text =
    match content "text/plain" with
    | Some text -> Ok text
    | None -> Or_error.error_s [%message "message has no text/plain part"]
  in
  let html = content "text/html" in
  let extra =
    List.filter headers ~f:(fun (name, _) ->
      not (is_reserved name || String.is_prefix (String.lowercase name) ~prefix:"content-"))
  in
  (* A message sent only to Bcc recipients has no recipient fields at all. *)
  make
    ~require_recipients:false
    ~from
    ~to_
    ~cc
    ?reply_to
    ~headers:extra
    ?html
    ~subject
    ~text
    ()
;;
