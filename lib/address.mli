(** Email addresses: the RFC 5322 [addr-spec] ([local-part@domain]).

    Accepted local parts are dot-atoms ([first.last+tag]) and quoted strings
    (["first last"]); accepted domains are dot-atoms and domain literals ([[192.0.2.1]]).
    Comments, folding whitespace and the obsolete forms of RFC 5322 section 4 are refused,
    as are bytes outside ASCII (internationalised addresses need SMTPUTF8, which this
    library does not speak). RFC 5321 length limits apply: at most 64 octets before the
    [@] and 254 in all.

    The domain is kept in lower case, since domains are case-insensitive; the local part
    is kept as written, since only the receiving server may interpret its case. *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** [of_string s] validates [s]. Surrounding whitespace is an error, not trimmed. *)
val of_string : string -> t Or_error.t

(** [of_string_exn s] is {!of_string} raising on error. *)
val of_string_exn : string -> t

(** [to_string t] is the address as it goes into a header or an SMTP command. *)
val to_string : t -> string

(** [local_part t] is everything before the last [@], quotes included. *)
val local_part : t -> string

(** [domain t] is everything after the last [@], in lower case. *)
val domain : t -> string

(** [redacted t] shows the first character of the local part and the domain
    ([a***@example.com]), for logs. *)
val redacted : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
  end
end
