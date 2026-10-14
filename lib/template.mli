(** Typed email templates.

    A template is a set of OCaml functions, one per locale, from a parameter record to a
    subject, a text body and an optional HTML body. There is no template language: the
    compiler checks that every parameter exists and has the right type, and
    {!Catalogue.check} checks what the compiler cannot (every locale present, every sample
    renders, required values appear, HTML escaped and balanced). *)

open! Core

module Locale : sig
  (** A language tag: a language (["en"]) and an optional region (["pt-BR"]). Case is
      normalised: language lower, region upper. *)
  type t [@@deriving sexp, compare, equal, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  (** [of_string s] accepts [ll], [lll], [ll-RR] and [ll-999] (also with [_]). *)
  val of_string : string -> t Or_error.t

  (** [of_string_exn s] is {!of_string} raising on error. *)
  val of_string_exn : string -> t

  (** [to_string t] is the tag with a hyphen. *)
  val to_string : t -> string

  (** [language t] drops the region. *)
  val language : t -> t

  (** English. *)
  val en : t

  (** German. *)
  val de : t

  (** French. *)
  val fr : t

  (** Spanish. *)
  val es : t

  (** Italian. *)
  val it : t

  (** Portuguese. *)
  val pt : t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
    end
  end
end

(** What a template produces for one locale. *)
module Rendered : sig
  type t =
    { subject : string
    ; text : string
    ; html : string option
    }
  [@@deriving sexp, compare, equal]
end

module Html : sig
  (** [escape s] replaces ampersand, angle brackets and both quote characters with
      character references. Every parameter that goes into HTML must pass through it. *)
  val escape : string -> string
end

type 'params t

(** [create ~name ~samples ?must_mention renderers] makes a template. The first renderer's
    locale is the fallback for locales the template lacks. [samples] are parameter values
    the catalogue check renders with; [must_mention params] lists strings each rendering
    must contain (the code in a code email, say).

    Raises if [renderers] or [samples] is empty, [name] is empty, or a locale appears
    twice: these are mistakes in the program, not in its input. *)
val create
  :  name:string
  -> samples:'params list
  -> ?must_mention:('params -> string list)
  -> (Locale.t * ('params -> Rendered.t)) list
  -> 'params t

(** [name t] is the template's name. *)
val name : _ t -> string

(** [locales t] lists the locales with a renderer, fallback first. *)
val locales : _ t -> Locale.t list

(** [resolve t locale] is the locale that renders [locale]: the exact tag, then its
    language, then the fallback. *)
val resolve : _ t -> Locale.t -> Locale.t

(** [render t ~locale params] renders with the renderer {!resolve} picks. *)
val render : 'params t -> locale:Locale.t -> 'params -> Rendered.t

(** [to_message t ~locale params ~from ~to_] renders and builds a message. *)
val to_message
  :  'params t
  -> locale:Locale.t
  -> 'params
  -> from:Mailbox.t
  -> to_:Mailbox.t list
  -> ?headers:(string * string) list
  -> unit
  -> Message.t Or_error.t

module Catalogue : sig
  (** Templates of any parameter type, for checking together. *)
  type template = T : _ t -> template

  module Problem : sig
    type t =
      { template : string
      ; locale : Locale.t option
      ; problem : string
      }
    [@@deriving sexp, compare, equal]
  end

  (** [check ?required_locales templates] renders every template in every locale it has
      with every sample and reports:

      - template names used twice;
      - required locales without their own renderer (a fallback does not count);
      - renderers that raise;
      - empty subjects, subjects with line breaks or over 150 characters;
      - empty text bodies;
      - [must_mention] strings missing from the text, or from the HTML;
      - [must_mention] strings that appear in the HTML unescaped (when escaping would
        change them);
      - HTML whose tags do not nest or close.

      An empty list means the catalogue is sound. Run it in a test. *)
  val check : ?required_locales:Locale.t list -> template list -> Problem.t list
end
