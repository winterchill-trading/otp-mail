(** MIME building blocks: transfer encodings (RFC 2045), encoded words for non-ASCII
    header text (RFC 2047), header folding (RFC 5322 section 2.2.3), content types and a
    parser for whole entities including [multipart/*] bodies (RFC 2046).

    Everything here works on raw bytes. Line breaks on the wire are CRLF; the decoders
    also accept bare LF, which is what files on disk usually hold. *)

open! Core

(** Base64 as used for [Content-Transfer-Encoding: base64]. *)
module Base64 : sig
  (** [encode s] is the base64 form of [s], broken into lines of [line_length] characters
      (default 76, the RFC 2045 maximum) joined by CRLF, with no trailing line break.
      [~line_length:0] gives one unbroken line. [line_length] is rounded down to a
      multiple of 4. *)
  val encode : ?line_length:int -> string -> string

  (** [decode s] reverses {!encode}. Whitespace (including line breaks) is ignored;
      anything else outside the alphabet, or bad padding, is an error. *)
  val decode : string -> string Or_error.t
end

(** Quoted-printable, for text that is mostly ASCII. *)
module Quoted_printable : sig
  (** [encode s] is the quoted-printable form of [s] with lines of at most 76 characters.

      In the default text mode, CRLF and bare LF in [s] become CRLF line breaks on the
      wire, a lone CR is encoded as [=0D], and spaces or tabs before a line break are
      encoded so that transports which strip trailing whitespace cannot change the text.
      With [~binary:true] CR and LF are encoded too and the output is one logical line
      broken only by soft line breaks. *)
  val encode : ?binary:bool -> string -> string

  (** [decode s] reverses {!encode}. Hard line breaks come back as CRLF, soft line breaks
      ([=] at the end of a line) are removed, and trailing whitespace on a line is dropped
      as RFC 2045 requires. A malformed escape is an error. *)
  val decode : string -> string Or_error.t
end

(** Encoded words, the RFC 2047 way of carrying non-ASCII text in headers. *)
module Encoded_word : sig
  (** [needs_encoding s] holds when [s] cannot appear as-is in an unstructured header: it
      has bytes outside printable ASCII and space, or a ["=?"] that a reader would take
      for the start of an encoded word. *)
  val needs_encoding : string -> bool

  (** [encode s] is [s] itself when {!needs_encoding} is false, and otherwise a sequence
      of [=?UTF-8?B?...?=] words separated by single spaces. Each word is at most 75
      characters and never splits a UTF-8 sequence, so a folder can break the line between
      words. [s] is taken to be UTF-8. *)
  val encode : string -> string

  (** [decode s] replaces every encoded word in [s] (B or Q encoding; charsets UTF-8,
      US-ASCII and ISO-8859-1, the last converted to UTF-8) with its text. Whitespace
      between two adjacent encoded words is dropped, as RFC 2047 section 6.2 requires.
      Words that cannot be decoded are left as they are. *)
  val decode : string -> string
end

(** Header fields. *)
module Header : sig
  (** A header section in wire order. Names keep the case they were written with. *)
  type t = (string * string) list [@@deriving sexp_of]

  (** [valid_name s] holds for a non-empty field name of printable ASCII without a colon. *)
  val valid_name : string -> bool

  (** [fold ~name value] is the field ["name: value"] followed by CRLF, folded by
      inserting CRLF before a space or tab so that lines stay within 78 characters where
      the value allows it. A run without whitespace longer than a line is left whole (RFC
      5322 allows up to 998 characters). [value] must not contain CR or LF; they are
      replaced by spaces so that a value can never inject another field. *)
  val fold : name:string -> string -> string

  (** [unfold s] removes every CRLF (or LF) that is followed by a space or tab. *)
  val unfold : string -> string

  (** [find t name] is the value of the first field called [name], compared without regard
      to case. *)
  val find : t -> string -> string option

  (** [find_all t name] is every value of the fields called [name], in order. *)
  val find_all : t -> string -> string list

  (** [parse s] reads a header section (everything before the first empty line) into
      fields with unfolded, whitespace-trimmed values. Lines without a colon are an error. *)
  val parse : string -> t Or_error.t

  (** [to_string t] is the header section with every field folded, without the empty line
      that ends it. *)
  val to_string : t -> string
end

(** [Content-Type] values. *)
module Content_type : sig
  type t =
    { type_ : string (** lower case, e.g. ["text"] *)
    ; subtype : string (** lower case, e.g. ["plain"] *)
    ; params : (string * string) list
    (** names in lower case, values unquoted, in the order written *)
    }
  [@@deriving sexp, compare, equal]

  (** [create ?params type_ subtype] lower-cases the type, subtype and parameter names. *)
  val create : ?params:(string * string) list -> string -> string -> t

  (** [mime_type t] is ["type/subtype"]. *)
  val mime_type : t -> string

  (** [param t name] is the value of parameter [name] (case-insensitive). *)
  val param : t -> string -> string option

  (** [parse s] reads a header value such as [text/plain; charset="utf-8"]. Comments in
      parentheses are not supported. *)
  val parse : string -> t Or_error.t

  (** [to_string t] writes parameters as tokens when they can be, quoted strings
      otherwise. *)
  val to_string : t -> string

  (** [text/plain], the RFC 2045 default when a part has no [Content-Type]. *)
  val default : t
end

(** A parsed MIME entity: a header section and a body that is either decoded content or a
    list of parts. *)
module Entity : sig
  type body =
    | Leaf of string
    (** content after its transfer encoding was removed; bytes in the part's charset *)
    | Multipart of t list (** the parts of a [multipart/*] body, preamble dropped *)

  and t =
    { headers : Header.t
    ; content_type : Content_type.t
    ; body : body
    }
  [@@deriving sexp_of]

  (** [parse s] reads a whole message or part. The header section ends at the first empty
      line (CRLF CRLF or LF LF). [quoted-printable] and [base64] bodies are decoded;
      [7bit], [8bit] and [binary] bodies are kept. A multipart body without its closing
      boundary is an error. *)
  val parse : string -> t Or_error.t

  (** [find t ~mime_type] is the first leaf (depth first) whose media type is [mime_type],
      e.g. ["text/html"]. *)
  val find : t -> mime_type:string -> t option

  (** [leaf_content t] is the decoded body of a leaf, [None] for a multipart entity. *)
  val leaf_content : t -> string option
end
