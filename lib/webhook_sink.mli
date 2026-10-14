(** Delivery events that an API-based mailer reports by webhook.

    Email APIs accept a message over HTTPS and later post what happened to it (delivered,
    bounced, complained) to a URL of yours. Each API has its own payload and signature
    scheme, so this library leaves the parsing to an implementation of {!S} and provides
    the parts every implementation needs: an HMAC check over the raw body in constant
    time, and {!Sink}, which drops repeated deliveries and publishes the rest to a
    {!Mailer.Event_feed.t}.

    Verify the signature on the raw bytes before parsing anything, and answer the HTTP
    request only after {!Sink.receive} returns. *)

open! Core

module type S = sig
  type t

  (** [parse t ~headers ~body] checks the delivery's signature over [body] and returns its
      events, or an error if the signature or the payload is bad. *)
  val parse
    :  t
    -> headers:(string * string) list
    -> body:string
    -> Mailer.Event.t list Or_error.t
end

(** [hmac_sha256_hex ~key data] is the lower-case hex HMAC-SHA256 of [data]. *)
val hmac_sha256_hex : key:string -> string -> string

(** [verify_hmac_sha256 ~key ~body ~signature] compares [signature] (hex, either case)
    with the HMAC-SHA256 of [body] in constant time. *)
val verify_hmac_sha256 : key:string -> body:string -> signature:string -> bool

module Sink : sig
  type t

  (** [create ?capacity feed] publishes to [feed] and remembers the last [capacity] events
      (default 100 000) to drop repeats. *)
  val create : ?capacity:int -> Mailer.Event_feed.t -> t

  (** [receive t (module P) p ~headers ~body] parses with [P] and publishes the events not
      seen before; it answers how many were new. *)
  val receive
    :  t
    -> (module S with type t = 'a)
    -> 'a
    -> headers:(string * string) list
    -> body:string
    -> int Or_error.t
end
