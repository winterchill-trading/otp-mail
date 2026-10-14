(** Transactional email messages: one text body and an optional HTML alternative.

    {!render} writes RFC 5322 with MIME: UTF-8 everywhere, non-ASCII header text as
    encoded words, bodies as 7bit when they are short-lined ASCII and quoted-printable
    otherwise, and [multipart/alternative] (text first, then HTML) when there is HTML.
    {!of_wire} reads such a message back; [of_wire (render t)] gives [t] without its
    [Bcc]. *)

open! Core

type t = private
  { from : Mailbox.t
  ; to_ : Mailbox.t list
  ; cc : Mailbox.t list
  ; bcc : Mailbox.t list (** envelope recipients only; never rendered *)
  ; reply_to : Mailbox.t option
  ; subject : string
  ; text : string (** line breaks are LF *)
  ; html : string option (** line breaks are LF *)
  ; headers : (string * string) list
  (** extra fields in order, e.g. [List-Unsubscribe] or [X-Entity-Ref-ID] *)
  }
[@@deriving sexp, compare, equal]

(** [create] checks the message:

    - at least one recipient in [to_], [cc] or [bcc];
    - a subject without CR or LF (surrounding whitespace is trimmed);
    - extra header names that are valid and not one of the fields this module writes
      itself ([From], [To], [Cc], [Bcc], [Reply-To], [Subject], [Date], [Message-ID],
      [MIME-Version], [Content-Type], [Content-Transfer-Encoding], [DKIM-Signature]);
    - extra header values of printable ASCII (trimmed).

    Line breaks in [text] and [html] (CRLF, CR or LF) are stored as LF. *)
val create
  :  from:Mailbox.t
  -> ?to_:Mailbox.t list
  -> ?cc:Mailbox.t list
  -> ?bcc:Mailbox.t list
  -> ?reply_to:Mailbox.t
  -> ?headers:(string * string) list
  -> ?html:string
  -> subject:string
  -> text:string
  -> unit
  -> t Or_error.t

(** [recipients t] is every address in [to_], [cc] and [bcc], first occurrence kept. *)
val recipients : t -> Address.t list

(** [render t ~date ~message_id] is the message on the wire, with CRLF line breaks. The
    MIME boundary is derived from the content, so the same inputs always give the same
    bytes. *)
val render : t -> date:Time_ns.t -> message_id:Message_id.t -> string

(** [rfc5322_date time] is [time] in UTC as RFC 5322 writes dates, e.g.
    [Wed, 14 Oct 2026 09:30:00 +0000]. *)
val rfc5322_date : Time_ns.t -> string

(** [of_wire s] parses a message: the addresses, the subject (encoded words decoded), the
    first [text/plain] and [text/html] leaves, and every other header field except [Date],
    [Message-ID], [MIME-Version], [Content-*] and [DKIM-Signature]. *)
val of_wire : string -> t Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, equal, stable_witness]
  end
end
