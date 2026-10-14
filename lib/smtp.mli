(** A minimal SMTP submission client (RFC 5321, RFC 6409) as a {!Mailer.S}.

    One connection per message: connect, [EHLO], [STARTTLS] when configured, [AUTH]
    ([PLAIN], else [LOGIN]), [MAIL FROM], one [RCPT TO] per envelope recipient, [DATA],
    [QUIT]. Messages are rendered by {!Message.render} (7-bit, so neither [8BITMIME] nor
    [SMTPUTF8] is needed) and optionally DKIM-signed.

    TLS goes through async_ssl (OpenSSL). Certificates are checked against the system
    roots (or [ca_file]) and the host name is checked against the certificate. [Starttls]
    refuses to continue when the server does not offer [STARTTLS], so a stripped
    capability cannot downgrade the session, and credentials are never sent without TLS
    unless [allow_plaintext_auth] says so (for a relay on localhost).

    Not covered: connection reuse and pipelining, [CHUNKING], DSN parameters, MX lookup
    (this is a submission client: it talks to one relay). *)

open! Core
open! Async

module Security : sig
  type t =
    | Plain (** no TLS: only for a relay on the same host or a test server *)
    | Starttls (** port 587: plain connection upgraded with [STARTTLS] *)
    | Implicit_tls (** port 465: TLS from the first byte *)
  [@@deriving sexp, compare, equal]

  (** [of_string s] reads ["plain"], ["starttls"] or ["tls"]. *)
  val of_string : string -> t Or_error.t
end

module Credentials : sig
  (** Its sexp shows the user name only. *)
  type t [@@deriving sexp_of]

  (** [create ~username ~password] *)
  val create : username:string -> password:string -> t
end

module Config : sig
  type t =
    { host : string (** the relay; also the name its certificate must carry *)
    ; port : int
    ; security : Security.t
    ; credentials : Credentials.t option
    ; helo_name : string (** the name sent with [EHLO] *)
    ; ca_file : string option
    (** trusted roots in PEM; default: the first of [SSL_CERT_FILE],
        [/etc/ssl/certs/ca-certificates.crt], [/etc/ssl/cert.pem] and the Homebrew OpenSSL
        bundle that exists *)
    ; timeout : Time_ns.Span.t (** per connection step and per command *)
    ; allow_plaintext_auth : bool
    ; message_id_domain : string (** right half of generated [Message-ID]s *)
    ; dkim : Dkim.Config.t option
    }
  [@@deriving sexp_of]

  (** [create ~host ~message_id_domain ()] defaults to [Starttls] on port 587 (465 for
      [Implicit_tls], 25 for [Plain]), [EHLO localhost], a 30 s timeout and no
      credentials. *)
  val create
    :  ?port:int
    -> ?security:Security.t
    -> ?credentials:Credentials.t
    -> ?helo_name:string
    -> ?ca_file:string
    -> ?timeout:Time_ns.Span.t
    -> ?allow_plaintext_auth:bool
    -> ?dkim:Dkim.Config.t
    -> host:string
    -> message_id_domain:string
    -> unit
    -> t

  (** [of_env ()] reads [OTP_MAIL_SMTP_HOST] (required), [OTP_MAIL_SMTP_PORT],
      [OTP_MAIL_SMTP_SECURITY] ([plain], [starttls] or [tls]), [OTP_MAIL_SMTP_USERNAME]
      and [OTP_MAIL_SMTP_PASSWORD], [OTP_MAIL_SMTP_HELO], [OTP_MAIL_SMTP_CA_FILE] and
      [OTP_MAIL_MESSAGE_ID_DOMAIN] (default: the host). *)
  val of_env : unit -> t Or_error.t
end

(** A server reply. *)
module Reply : sig
  type t =
    { code : int
    ; lines : string list (** text after the code, one entry per line *)
    }
  [@@deriving sexp, compare, equal]

  (** [to_string t] is ["250 first line"] (further lines joined with ["; "]). *)
  val to_string : t -> string
end

type t

(** [create config] sends with [config]. [trace] sees every line exchanged (without
    message content, and with [AUTH] arguments replaced by [<redacted>]). *)
val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?rng:Rng.t
  -> ?trace:([ `Client | `Server ] -> string -> unit)
  -> Config.t
  -> t

include Mailer.S with type t := t

(** [mailer t] is [t] as a {!Mailer.t}. *)
val mailer : t -> Mailer.t

(** [send_wire t ~from ~recipients wire] runs one SMTP transaction for an already rendered
    message. On success it answers the server's reply to the message data and the
    recipients the server refused (when it accepted at least one). *)
val send_wire
  :  t
  -> from:Address.t
  -> recipients:Address.t list
  -> string
  -> (Reply.t * (Address.t * Reply.t) list, Mailer.Send_error.t) Result.t Deferred.t

(** [dot_stuff wire] is [wire] as it goes after [DATA]: CRLF line breaks, a ["."] before
    each line that starts with one, a final CRLF, and the terminating [".\r\n"]. *)
val dot_stuff : string -> string
