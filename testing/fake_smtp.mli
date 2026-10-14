(** A small SMTP server for tests: it listens on 127.0.0.1, speaks enough ESMTP for
    {!Otp_mail.Smtp} (EHLO, STARTTLS, AUTH PLAIN and LOGIN, MAIL, RCPT, DATA, RSET, NOOP,
    QUIT), keeps every message it accepts, and can be told to refuse things. It never
    relays anything. *)

open! Core
open! Async

module Received : sig
  type t =
    { mail_from : string
    ; rcpt_to : string list
    ; data : string (** after dot-unstuffing, CRLF line breaks, without the final "." *)
    ; tls : bool (** whether the session was encrypted when DATA was sent *)
    ; authenticated_as : string option
    }
  [@@deriving sexp_of]
end

type t

(** [start ()] listens on an ephemeral port.

    - [tls] (certificate file, key file) enables STARTTLS, or with [implicit_tls] TLS from
      the first byte;
    - [advertise_starttls] (default: whether [tls] is given) lets a test hide the
      capability to check that clients refuse to continue;
    - [credentials] (user, password) makes AUTH required before MAIL; the mechanisms
      offered are [auth_mechanisms] (default [PLAIN LOGIN]);
    - [recipient_reply address] can refuse a recipient with a code and text;
    - [data_reply] replaces the final 250 after DATA (e.g. a 451 or a 554);
    - [greeting] replaces the 220 greeting (e.g. a 554 to refuse the session). *)
val start
  :  ?tls:string * string
  -> ?implicit_tls:bool
  -> ?advertise_starttls:bool
  -> ?credentials:string * string
  -> ?auth_mechanisms:string list
  -> ?recipient_reply:(string -> (int * string) option)
  -> ?data_reply:int * string
  -> ?greeting:int * string
  -> unit
  -> t Deferred.t

(** [port t] is the port it listens on. *)
val port : t -> int

(** [received t] is every accepted message, oldest first. *)
val received : t -> Received.t list

(** [transcript t] is every command line it read, oldest first, with AUTH arguments
    replaced by [<redacted>] and message data left out. *)
val transcript : t -> string list

(** [close t] stops listening. *)
val close : t -> unit Deferred.t
