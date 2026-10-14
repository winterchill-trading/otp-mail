(** Sending email, whatever does the sending.

    A mailer takes a {!Message.t}, answers with a {!Delivery_id.t} once the message is
    accepted, and reports what happens next as {!Event.t}s: an SMTP relay can only say
    [Sent] or [Failed]; an API-based provider also reports [Delivered], [Bounced] and
    [Complained] through webhooks (see {!Webhook_sink}).

    Implementations in this library: {!Simulated_inbox} (in memory, for tests and demos)
    and {!Smtp}. *)

open! Core
open! Async

module Delivery_id : sig
  (** The id a mailer gives an accepted message. For the mailers in this library it is the
      left half of the [Message-ID] they generated. *)
  type t [@@deriving sexp, compare, equal, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  (** [of_string s] needs a non-empty string of printable ASCII. *)
  val of_string : string -> t Or_error.t

  (** [to_string t] is the id. *)
  val to_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
    end
  end
end

module Status : sig
  type t =
    | Queued (** accepted by the mailer, not yet handed on *)
    | Sent (** accepted by the next hop (the relay or the provider) *)
    | Delivered (** accepted by the recipient's server *)
    | Deferred of { reason : string } (** a temporary failure; may still arrive *)
    | Bounced of { reason : string } (** permanently refused for this recipient *)
    | Complained (** the recipient marked it as spam *)
    | Failed of { reason : string } (** never left the mailer *)
  [@@deriving sexp, compare, equal]

  (** [is_final t] holds for [Delivered], [Bounced], [Complained] and [Failed]. *)
  val is_final : t -> bool

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end
end

module Event : sig
  type t =
    { delivery_id : Delivery_id.t
    ; recipient : Address.t option (** [None] when the status covers every recipient *)
    ; status : Status.t
    ; at : Time_ns.t
    }
  [@@deriving sexp, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end
end

module Send_error : sig
  type t =
    | Invalid_message of Error.t (** the mailer cannot send this message as it is *)
    | Rejected of
        { code : int
        ; message : string
        } (** a permanent refusal, e.g. an SMTP 5xx reply *)
    | Temporary of { message : string } (** try again later, e.g. an SMTP 4xx reply *)
    | Transport of Error.t (** connection, TLS or timeout trouble *)
  [@@deriving sexp_of]

  (** [is_retryable t] holds for [Temporary] and [Transport]. *)
  val is_retryable : t -> bool
end

module type S = sig
  type t

  (** [send t ?idempotency_key message] hands [message] over. Sending again with the same
      [idempotency_key] and the same message answers the first delivery id without
      sending; the same key with a different message is an [Invalid_message] error. Keys
      are remembered in memory by the mailer value, for its lifetime. *)
  val send
    :  t
    -> ?idempotency_key:string
    -> Message.t
    -> (Delivery_id.t, Send_error.t) Result.t Deferred.t

  (** [events t] is a new reader that receives every event from now on. *)
  val events : t -> Event.t Pipe.Reader.t
end

(** Any mailer. *)
type t

(** [pack (module M) m] hides which mailer [m] is. *)
val pack : (module S with type t = 'a) -> 'a -> t

(** [send t ?idempotency_key message] is [M.send]. *)
val send
  :  t
  -> ?idempotency_key:string
  -> Message.t
  -> (Delivery_id.t, Send_error.t) Result.t Deferred.t

(** [events t] is [M.events]. *)
val events : t -> Event.t Pipe.Reader.t

(** Building blocks for mailer implementations. *)
module Event_feed : sig
  (** Fans events out to every subscriber. *)
  type t

  (** [create ()] has no subscribers. *)
  val create : unit -> t

  (** [publish t event] writes [event] to every open subscriber without waiting. *)
  val publish : t -> Event.t -> unit

  (** [subscribe t] is a reader of events published from now on. Closing it unsubscribes. *)
  val subscribe : t -> Event.t Pipe.Reader.t
end

module Idempotency : sig
  (** Remembers which delivery id each idempotency key produced, for the last [capacity]
      keys. *)
  type t

  (** [create ?capacity ()] keeps up to [capacity] keys (default 10 000), forgetting the
      oldest first. *)
  val create : ?capacity:int -> unit -> t

  (** [run t ~key message ~send] calls [send] unless [key] was seen: with the same message
      it answers the remembered id, with another it is an error. Failed sends are not
      remembered, so they can be retried with the same key. Concurrent calls with one key
      send once. *)
  val run
    :  t
    -> key:string option
    -> Message.t
    -> send:(unit -> (Delivery_id.t, Send_error.t) Result.t Deferred.t)
    -> (Delivery_id.t, Send_error.t) Result.t Deferred.t
end
