(** [Message-ID] values (RFC 5322 section 3.6.4): [id-left@id-right], written in angle
    brackets in headers. *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

(** [create ~local ~domain] validates both halves as dot-atoms. *)
val create : local:string -> domain:string -> t Or_error.t

(** [of_string s] reads [<local@domain>] or [local@domain]. *)
val of_string : string -> t Or_error.t

(** [generate ~rng ~domain] uses 16 random bytes, hex encoded, as the left half. *)
val generate : rng:Rng.t -> domain:string -> t

(** [to_string t] is [<local@domain>]. *)
val to_string : t -> string

(** [local t] is the part before the [@]. *)
val local : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
  end
end
