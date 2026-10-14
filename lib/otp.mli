(** One-time codes: numeric codes sent by email or SMS to prove possession of an inbox or
    phone.

    This module is pure: every function takes the time, the random source and the stored
    {!Record.t} as arguments and returns the next record. {!Otp_store} describes where
    records live and {!Otp_service} ties the two together.

    Rules it enforces:

    - a code is drawn uniformly from a CSPRNG and never stored: the record keeps an
      HMAC-SHA256 of it under a server-side {!Key.t}, so a leaked store does not reveal
      codes, and 10[{^6}] guesses against a leaked hash need the key too;
    - each code has one purpose ({!Purpose.t}); a code for signing in does not reset a
      passcode, and an SCA code is bound to the operation it was shown for (amount and
      payee, PSD2 RTS article 5), so any change to the operation invalidates it;
    - a code is accepted once, until it expires;
    - consecutive failures are counted per subject and purpose, across re-sent codes, and
      reaching {!Config.max_attempts} (5 by default, the RTS article 4(3)(b) ceiling)
      locks the subject out for a period that doubles with each lockout;
    - comparisons run in constant time.

    Rejections carry a reason for logs and audit; the RTS (article 4(3)(a)) asks that what
    the user sees does not say which part of an authentication failed, so show one message
    for every rejection. *)

open! Core

module Purpose : sig
  module Kind : sig
    (** A purpose without its SCA binding: what records are keyed by. *)
    type t =
      | Sign_in
      | Passcode_reset
      | Sca
    [@@deriving sexp, compare, equal, hash, enumerate]

    (** ["sign-in"], ["passcode-reset"], ["sca"]. *)
    val to_string : t -> string

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
      end
    end
  end

  type t =
    | Sign_in
    | Passcode_reset
    | Sca of { binding : string }
    (** [binding] identifies the operation the code authorises, e.g. a digest over amount,
        currency and payee computed by the caller. Verification must present the same
        binding. *)
  [@@deriving sexp, compare, equal, hash]

  (** [kind t] forgets the binding. At most one code per subject and kind is outstanding. *)
  val kind : t -> Kind.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
    end
  end
end

(** The server-side secret that code hashes are keyed with. *)
module Key : sig
  (** Its sexp shows the id only. *)
  type t [@@deriving sexp_of]

  (** [create ~id ~secret] needs at least 32 bytes of secret. [id] is stored next to each
      hash so that keys can be rotated: verify with the old key in [previous_keys] until
      its codes have expired. *)
  val create : id:string -> secret:string -> t Or_error.t

  (** [of_env ?var ()] reads a hex secret from [var] (default [OTP_MAIL_OTP_KEY]) and an
      optional id from [var ^ "_ID"] (default ["k1"]). *)
  val of_env : ?var:string -> unit -> t Or_error.t

  (** [id t] is the key's id. *)
  val id : t -> string
end

(** A code as shown to the user. *)
module Code : sig
  (** Its sexp is [<redacted>]: codes never reach logs. *)
  type t [@@deriving sexp_of]

  (** [to_string t] is the digits, for the message that delivers the code. *)
  val to_string : t -> string
end

module Config : sig
  type t = private
    { digits : int (** code length, 4 to 10; default 6 *)
    ; ttl : Time_ns.Span.t (** lifetime of a code; default 10 minutes *)
    ; max_attempts : int (** failures before a lockout, 1 to 10; default 5 *)
    ; lockout_base : Time_ns.Span.t (** first lockout; default 1 minute *)
    ; lockout_max : Time_ns.Span.t (** longest lockout; default 1 hour *)
    ; resend_interval : Time_ns.Span.t
    (** minimum time between two codes for the same subject and kind; default 30 s *)
    }
  [@@deriving sexp_of, compare, equal]

  (** [create ()] checks the bounds above, that [ttl] is positive and that the other spans
      are not negative. Zero turns a wait off: [~resend_interval:zero] allows a new code
      at once and [~lockout_base:zero] makes lockouts end as they start (only the lockout
      count remains). Both are meant for tests and local demos; production settings should
      keep the defaults' order of magnitude. *)
  val create
    :  ?digits:int
    -> ?ttl:Time_ns.Span.t
    -> ?max_attempts:int
    -> ?lockout_base:Time_ns.Span.t
    -> ?lockout_max:Time_ns.Span.t
    -> ?resend_interval:Time_ns.Span.t
    -> unit
    -> t Or_error.t

  (** All defaults. *)
  val default : t

  (** [lockout_duration t ~lockouts] is how long the lockout that follows [lockouts]
      earlier ones lasts: [lockout_base * 2^lockouts], at most [lockout_max]. *)
  val lockout_duration : t -> lockouts:int -> Time_ns.Span.t
end

(** An outstanding code. *)
module Challenge : sig
  type t =
    { id : string (** random, 32 hex characters *)
    ; key_id : string
    ; code_hash : string (** hex HMAC-SHA256 over id, purpose and code *)
    ; purpose : Purpose.t
    ; issued_at : Time_ns.t
    ; expires_at : Time_ns.t
    }
  [@@deriving sexp, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end
end

(** Everything stored for one subject and purpose kind. *)
module Record : sig
  type t =
    { subject : string (** whom the code is for, e.g. a customer id *)
    ; kind : Purpose.Kind.t
    ; version : int (** 0 for a record never saved; {!Otp_store.S.save} bumps it by one *)
    ; challenge : Challenge.t option
    ; failures : int (** consecutive failed attempts since the last success or lockout *)
    ; lockouts : int (** lockouts since the last success *)
    ; locked_until : Time_ns.t option
    ; last_issued_at : Time_ns.t option
    }
  [@@deriving sexp, compare, equal]

  (** [empty ~subject ~kind] is the record of a subject that never had a code. *)
  val empty : subject:string -> kind:Purpose.Kind.t -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end
end

module Issue_error : sig
  type t =
    | Locked of { until : Time_ns.t }
    | Too_soon of { retry_at : Time_ns.t }
  [@@deriving sexp, compare, equal]
end

(** A newly issued code. *)
module Issued : sig
  type t =
    { challenge_id : string
    ; code : Code.t
    ; expires_at : Time_ns.t
    }
  [@@deriving sexp_of]
end

module Rejection : sig
  type t =
    | No_challenge (** nothing outstanding, or the code was already used *)
    | Expired
    | Wrong_code (** wrong digits, or a code issued for a different purpose or binding *)
    | Locked_out of { until : Time_ns.t } (** this failure started a lockout *)
    | Locked of { until : Time_ns.t } (** a lockout was already running *)
  [@@deriving sexp, compare, equal]
end

module Verify_result : sig
  type t =
    | Verified
    | Rejected of Rejection.t
  [@@deriving sexp, compare, equal]
end

(** [issue config ~key ~rng ~now ~purpose record] draws a new code, replacing any
    outstanding one for the same kind. It is refused while the subject is locked out and
    within [resend_interval] of the previous code. Failures carry over to the new code.
    Returns the next record (same [version]; the store bumps it). *)
val issue
  :  Config.t
  -> key:Key.t
  -> rng:Rng.t
  -> now:Time_ns.t
  -> purpose:Purpose.t
  -> Record.t
  -> (Record.t * Issued.t, Issue_error.t) Result.t

(** [verify config ~keys ~now ~purpose ~code record] checks [code] (spaces and hyphens
    ignored) against the outstanding challenge. A success consumes the challenge and
    clears failures and lockouts; a wrong code counts a failure and may start a lockout;
    an expired or missing challenge or a running lockout counts nothing. [keys] must
    include the key the challenge was issued with. *)
val verify
  :  Config.t
  -> keys:Key.t list
  -> now:Time_ns.t
  -> purpose:Purpose.t
  -> code:string
  -> Record.t
  -> Record.t * Verify_result.t

(** The outcome of {!verify_challenge}: {!Rejection.t} with the number of attempts left on
    a wrong code. *)
module Attempt : sig
  type t =
    | Verified
    | No_challenge
    (** no outstanding challenge with this id: never issued, replaced by a newer code,
        already used, or its key retired *)
    | Expired
    | Wrong_code of { attempts_left : int }
    (** wrong digits, or a code for another purpose or binding; [attempts_left] more
        failures are allowed before a lockout (at least 1) *)
    | Locked_out of { until : Time_ns.t }
    (** this failure started a lockout; the challenge is kept *)
    | Locked of { until : Time_ns.t } (** a lockout was already running *)
  [@@deriving sexp, compare, equal]

  (** The same outcome without the attempt count, as {!verify} reports it. *)
  val to_verify_result : t -> Verify_result.t
end

(** [attempts_left config record] is how many wrong codes the subject may still enter
    before a lockout: [max_attempts] for a fresh record. Show it with a newly issued code
    if the interface counts attempts down. *)
val attempts_left : Config.t -> Record.t -> int

(** [verify_challenge config ~keys ~now ~challenge_id ~purpose ~code record] checks [code]
    against the outstanding challenge, which must have id [challenge_id]: a client that
    holds an older challenge id gets [No_challenge] and nothing is counted. It otherwise
    follows {!verify}, with two differences:

    - a wrong code reports how many attempts are left. The limit is {!Config.max_attempts}
      per code, and failures carry over to a re-sent code, so asking for a new code never
      adds attempts;
    - a lockout keeps the challenge, so its id still resolves (to [Locked] while the
      lockout runs) and the same code may be tried again after it, until the code expires.
      Lockouts double each time, which bounds the guesses one code can take.

    Running a lockout is checked before expiry, so a locked subject hears [Locked] even
    for an expired code. *)
val verify_challenge
  :  Config.t
  -> keys:Key.t list
  -> now:Time_ns.t
  -> challenge_id:string
  -> purpose:Purpose.t
  -> code:string
  -> Record.t
  -> Record.t * Attempt.t
