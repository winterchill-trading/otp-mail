(** A mailer that delivers into memory.

    Every message is rendered to the bytes an SMTP server would receive (DKIM-signed if
    configured) and parsed back, so tests see what a real recipient would. Queries find
    messages by recipient or subject, and {!next} waits for the next one, which is how an
    end-to-end test reads a one-time code out of an "inbox".

    Statuses after [Sent] come from [outcome], so bounces and complaints can be simulated
    per address. Nothing leaves the process. *)

open! Core
open! Async

type t

(** [create ()] makes an empty inbox.

    - [domain] (default ["simulated.invalid"]) is the right half of generated
      [Message-ID]s;
    - [outcome recipient] lists the statuses reported for [recipient] after [Sent]
      (default [[Delivered]]);
    - [dkim] signs each message before it is stored;
    - [time_source] and [rng] default to the wall clock and {!Rng.system}. *)
val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> ?rng:Rng.t
  -> ?domain:string
  -> ?dkim:Dkim.Config.t
  -> ?outcome:(Address.t -> Mailer.Status.t list)
  -> unit
  -> t

include Mailer.S with type t := t

(** [mailer t] is [t] as a {!Mailer.t}. *)
val mailer : t -> Mailer.t

module Received : sig
  type t =
    { delivery_id : Mailer.Delivery_id.t
    ; envelope_from : Address.t
    ; recipients : Address.t list (** every envelope recipient, [Bcc] included *)
    ; wire : string (** the bytes as sent, CRLF line breaks *)
    ; message : Message.t (** [wire] parsed back *)
    ; received_at : Time_ns.t
    }
  [@@deriving sexp_of]
end

(** [messages ?to_ ?subject_contains t] lists stored messages, oldest first, sent to [to_]
    (any envelope recipient) and whose subject contains [subject_contains]. *)
val messages : ?to_:Address.t -> ?subject_contains:string -> t -> Received.t list

(** [latest t ~to_] is the newest message for [to_]. *)
val latest : t -> to_:Address.t -> Received.t option

(** [next t ~to_] becomes determined with the first message for [to_] that arrives after
    the call. *)
val next : t -> to_:Address.t -> Received.t Deferred.t

(** [find t id] is the message with delivery id [id]. *)
val find : t -> Mailer.Delivery_id.t -> Received.t option

(** [count t] is the number of stored messages. *)
val count : t -> int

(** [clear t] forgets every stored message. Idempotency keys are kept. *)
val clear : t -> unit
