(** A mailbox: an address with an optional display name, as in
    [From: "Example Bank" <no-reply@bank.example>]. *)

open! Core

type t = private
  { display_name : string option (** UTF-8, without control characters *)
  ; address : Address.t
  }
[@@deriving sexp, compare, equal, hash]

(** [create ?display_name address] builds a mailbox. Control characters in the display
    name (CR and LF in particular, which could inject a header) become spaces, and the
    name is trimmed; an empty name is dropped. *)
val create : ?display_name:string -> Address.t -> t

(** [of_address a] is a mailbox without a display name. *)
val of_address : Address.t -> t

(** [to_string t] is the header form. A display name of atoms is written as it is, one
    with special characters as a quoted string, and one with non-ASCII text as RFC 2047
    encoded words. *)
val to_string : t -> string

(** [of_string s] reads [addr], [Name <addr>], ["Quoted, Name" <addr>] or
    [=?UTF-8?B?...?= <addr>]. *)
val of_string : string -> t Or_error.t

(** [list_to_string ts] joins header forms with [", "]. *)
val list_to_string : t list -> string

(** [list_of_string s] reads a comma-separated list, as in a [To] header. Commas inside
    quotes or angle brackets do not separate. Group syntax is not supported. *)
val list_of_string : string -> t list Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
  end
end
