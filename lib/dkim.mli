(** DKIM signatures (RFC 6376) with [rsa-sha256].

    {!sign} adds a [DKIM-Signature] field to a rendered message. The receiving side looks
    the public key up in DNS at [<selector>._domainkey.<domain>];
    {!Public_key.to_dns_record} gives the TXT record to publish. {!verify} checks a
    signature against a known public key without DNS, for tests and diagnostics; it is not
    a full DKIM verifier (it reads only the first signature and ignores [l=], [i=] and
    [x=]).

    Keys are 1024 bits or more; RFC 8301 requires at least 1024 and recommends 2048.
    [ed25519-sha256] (RFC 8463) is not provided. *)

open! Core

module Public_key : sig
  type t [@@deriving sexp_of]

  (** [bits t] is the modulus size. *)
  val bits : t -> int

  (** [of_dns_record s] reads the [p=] tag of a DKIM key record
      ([v=DKIM1; k=rsa; p=MIIB...]). The key may be a SubjectPublicKeyInfo or a bare
      PKCS#1 RSAPublicKey. *)
  val of_dns_record : string -> t Or_error.t

  (** [to_dns_record t] is [v=DKIM1; k=rsa; p=<base64 SubjectPublicKeyInfo>]. *)
  val to_dns_record : t -> string
end

module Private_key : sig
  (** A private key. Its sexp is [<redacted>] with the size. *)
  type t [@@deriving sexp_of]

  (** [of_pem s] reads a PKCS#8 ([BEGIN PRIVATE KEY]) or PKCS#1 ([BEGIN RSA PRIVATE KEY])
      unencrypted RSA key. Keys under 1024 bits are refused. *)
  val of_pem : string -> t Or_error.t

  (** [bits t] is the modulus size. *)
  val bits : t -> int

  (** [public_key t] is the matching public key. *)
  val public_key : t -> Public_key.t
end

module Canonicalization : sig
  (** [Simple] tolerates almost no change in transit; [Relaxed] survives whitespace
      changes and header re-folding, and is what most senders use. *)
  type t =
    | Simple
    | Relaxed
  [@@deriving sexp, compare, equal]

  (** [header t ~name ~value] is the canonical form of one field, CRLF included. [value]
      is everything after the colon, as on the wire. *)
  val header : t -> name:string -> value:string -> string

  (** [body t body] is the canonical form of a message body. *)
  val body : t -> string -> string
end

module Config : sig
  type t =
    { domain : string (** [d=], the signing domain *)
    ; selector : string (** [s=] *)
    ; key : Private_key.t
    ; header_canonicalization : Canonicalization.t
    ; body_canonicalization : Canonicalization.t
    ; headers : string list
    (** fields to sign, in order; names not present in a message are skipped *)
    ; expire_after : Time_ns.Span.t option (** sets [x=] when present *)
    }
  [@@deriving sexp_of]

  (** Fields signed by default: [From], [Reply-To], [Subject], [Date], [To], [Cc],
      [Message-ID], [MIME-Version], [Content-Type], [Content-Transfer-Encoding] and
      [List-Unsubscribe]. *)
  val default_headers : string list

  (** [create ~domain ~selector key] uses relaxed/relaxed, {!default_headers} and no
      expiry. *)
  val create
    :  ?header_canonicalization:Canonicalization.t
    -> ?body_canonicalization:Canonicalization.t
    -> ?headers:string list
    -> ?expire_after:Time_ns.Span.t
    -> domain:string
    -> selector:string
    -> Private_key.t
    -> t
end

(** [sign config ~now message] is [message] with a [DKIM-Signature] field in front.
    [message] must use CRLF line breaks, as {!Message.render} produces. [From] is listed
    once more in [h=] than it occurs, so that a [From] added in transit breaks the
    signature (RFC 6376 section 8.15). *)
val sign : Config.t -> now:Time_ns.t -> string -> string Or_error.t

(** [verify key message] checks the first [DKIM-Signature] of [message]: the body hash,
    then the RSA signature over the signed fields. *)
val verify : Public_key.t -> string -> unit Or_error.t
