(** A {!Mailer.S} for email APIs that take a message as JSON over HTTPS.

    Most transactional email services accept [POST <endpoint>] with a bearer token and a
    JSON body describing the message, and answer with a JSON body naming the message's id.
    They differ in field names, so the body is built and the answer read by a {!Shape.t}:
    {!Shape.fields} sends the message as fields (from, to, subject, text, HTML, headers),
    {!Shape.raw_mime} sends the rendered (and optionally DKIM-signed) message as base64,
    and a service with another layout gets its own [Shape.t] of a few lines.

    The delivery id is the id the service returns, so the delivery events its webhooks
    report (see {!Webhook_sink}) match the id {!send} answered. When the answer names no
    id, or one that is not a valid {!Mailer.Delivery_id.t}, the left half of the generated
    [Message-ID] is used, as for {!Smtp}.

    Answers map to {!Mailer.Send_error.t}: 2xx is success; 408, 429 and 5xx are
    [Temporary]; other 4xx are [Rejected] with the status; connection, TLS and timeout
    trouble, and unreadable answers, are [Transport]. A [Retry-After] header is not acted
    on; retrying is the caller's choice ({!Mailer.Send_error.is_retryable}).

    The HTTP client is minimal: HTTP/1.1, one connection per message
    ([Connection: close]), [Content-Length] or chunked answers, no redirects, no proxies.
    TLS goes through async_ssl with the same certificate and host name checks as {!Smtp}.
    Plain [http://] endpoints are accepted only for loopback hosts, for tests and local
    relays. *)

open! Core
open! Async

(** Where requests go. *)
module Endpoint : sig
  type t [@@deriving sexp_of, compare, equal]

  (** [of_string url] reads [https://host[:port]/path[?query]], or [http://] with host
      [localhost], [127.0.0.1] or [[::1]]. User info and fragments are refused. *)
  val of_string : string -> t Or_error.t

  (** [to_string t] is the URL. *)
  val to_string : t -> string

  (** [host t] is the host name (also the name the certificate must carry). *)
  val host : t -> string

  (** [port t] is the port: as given, else 443 for [https] and 80 for [http]. *)
  val port : t -> int

  (** [is_tls t] holds for [https]. *)
  val is_tls : t -> bool
end

(** An API token. *)
module Token : sig
  (** Its sexp is [<redacted>]. *)
  type t [@@deriving sexp_of]

  (** [of_string s] needs a non-empty token of visible ASCII (it goes into a header). *)
  val of_string : string -> t Or_error.t

  (** [of_env ?var ()] reads the token from [var] (default [OTP_MAIL_HTTP_API_TOKEN]). *)
  val of_env : ?var:string -> unit -> t Or_error.t
end

(** How a message becomes a request body and how an answer is read. *)
module Shape : sig
  type t =
    { encode : Message.t -> message_id:Message_id.t -> wire:string -> Yojson.Safe.t
    (** the request body. [wire] is the message as rendered for SMTP, DKIM-signed when the
        config has a DKIM key; shapes that send fields can ignore it. *)
    ; delivery_id : Yojson.Safe.t -> string option
    (** the service's id for the message, from a 2xx answer *)
    ; error_message : Yojson.Safe.t -> string option
    (** a readable reason from an error answer; the raw body (shortened) otherwise *)
    }

  (** The message as fields:

      {v
      {"from": {"email": "...", "name": "..."},
       "to": [{"email": "..."}], "cc": [...], "bcc": [...],
       "reply_to": {"email": "..."},
       "subject": "...", "text": "...", "html": "...",
       "headers": {"Message-ID": "<...>", ...}}
      v}

      Empty lists, a missing reply-to and a missing HTML body are left out; ["name"] only
      appears with a display name. The id is read from [id_field] (default ["id"]) at the
      top level of the answer, and errors from ["message"], or ["error"] when it is a
      string or an object with a ["message"]. *)
  val fields : ?id_field:string -> unit -> t

  (** The rendered message as [{"<field>": "<base64 of the wire bytes>"}] (default field
      ["raw"]), plus ["to"], a list of every envelope recipient's address, Bcc included.
      Ids and errors are read as in {!fields}. *)
  val raw_mime : ?field:string -> ?id_field:string -> unit -> t
end

module Config : sig
  type t =
    { endpoint : Endpoint.t
    ; token : Token.t option (** sent as [Authorization: Bearer <token>] *)
    ; timeout : Time_ns.Span.t (** for the whole exchange *)
    ; ca_file : string option (** trusted roots in PEM; default as for {!Smtp.Config.t} *)
    ; idempotency_header : string option
    (** the request header that carries {!send}'s [idempotency_key], when the service
        supports one *)
    ; headers : (string * string) list (** extra request headers *)
    ; message_id_domain : string (** right half of generated [Message-ID]s *)
    ; dkim : Dkim.Config.t option (** signs [wire] before {!Shape.t.encode} sees it *)
    ; max_response_bytes : int (** longer answers are a [Transport] error *)
    }
  [@@deriving sexp_of]

  (** [create ~endpoint ~message_id_domain ()] defaults to no token, a 30 s timeout, the
      [Idempotency-Key] header, no extra headers, no DKIM and answers up to 1 MiB. Raises
      if an extra header's name is not a token or its value holds CR or LF. *)
  val create
    :  ?token:Token.t
    -> ?timeout:Time_ns.Span.t
    -> ?ca_file:string
    -> ?idempotency_header:string option
    -> ?headers:(string * string) list
    -> ?dkim:Dkim.Config.t
    -> ?max_response_bytes:int
    -> endpoint:Endpoint.t
    -> message_id_domain:string
    -> unit
    -> t

  (** [of_env ()] reads [OTP_MAIL_HTTP_API_URL] (required), [OTP_MAIL_HTTP_API_TOKEN],
      [OTP_MAIL_HTTP_API_CA_FILE], [OTP_MAIL_HTTP_API_TIMEOUT_SECONDS] and
      [OTP_MAIL_MESSAGE_ID_DOMAIN] (default: the endpoint's host). *)
  val of_env : unit -> t Or_error.t
end

type t

(** [create config] sends with [config] and [shape] (default [Shape.fields ()]). [trace]
    sees the request line, the answer's status line and the outcome, never the token or
    the message. *)
val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?rng:Rng.t
  -> ?shape:Shape.t
  -> ?trace:(string -> unit)
  -> Config.t
  -> t

include Mailer.S with type t := t

(** [mailer t] is [t] as a {!Mailer.t}. *)
val mailer : t -> Mailer.t

(** An HTTP answer, as {!parse_response} reads it. *)
module Response : sig
  type t =
    { status : int
    ; headers : (string * string) list (** names lower-cased, in order *)
    ; body : string (** de-chunked *)
    }
  [@@deriving sexp_of]
end

(** [parse_response bytes] reads a whole HTTP/1.x answer: the status line, the header
    fields and a body delimited by [Content-Length], by chunked encoding, or by the end of
    the bytes. Exposed for tests. *)
val parse_response : string -> Response.t Or_error.t
