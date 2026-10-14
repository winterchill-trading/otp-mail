(** One-time codes over a store: load the record, apply {!Otp.issue} or {!Otp.verify},
    save it back, and retry when another request saved first. *)

open! Core
open! Async

module Make (Store : Otp_store.S) : sig
  type t

  (** [create ~key store] issues codes under [key] and also accepts codes issued under
      [previous_keys]. [time_source] defaults to the wall clock and [rng] to
      {!Rng.system}. *)
  val create
    :  ?config:Otp.Config.t
    -> ?rng:Rng.t
    -> ?time_source:[> read ] Time_source.T1.t
    -> ?previous_keys:Otp.Key.t list
    -> key:Otp.Key.t
    -> Store.t
    -> t

  (** [issue t ~subject ~purpose] draws a code. Send [code] to the subject and keep
      [challenge_id] if you want to correlate audit records. *)
  val issue
    :  t
    -> subject:string
    -> purpose:Otp.Purpose.t
    -> (Otp.Issued.t, Otp.Issue_error.t) Result.t Deferred.t

  (** [verify t ~subject ~purpose ~code] checks a code typed by the subject. *)
  val verify
    :  t
    -> subject:string
    -> purpose:Otp.Purpose.t
    -> code:string
    -> Otp.Verify_result.t Deferred.t

  (** [verify_challenge t ~subject ~challenge_id ~purpose ~code] is
      {!Otp.verify_challenge} over the store: the code must belong to the outstanding
      challenge [challenge_id], and a wrong code reports the attempts left. *)
  val verify_challenge
    :  t
    -> subject:string
    -> challenge_id:string
    -> purpose:Otp.Purpose.t
    -> code:string
    -> Otp.Attempt.t Deferred.t
end

(** {!Make} over a store that finds records by challenge id, for clients that send back
    the challenge id and the code only. *)
module Make_indexed (Store : Otp_store.Indexed) : sig
  include module type of Make (Store)

  (** [verify_by_challenge t ~challenge_id ~purpose ~code] finds the subject of the
      outstanding challenge [challenge_id], then runs {!verify_challenge}. It answers the
      subject when one was found, and [No_challenge] (counting nothing) when the id is
      unknown or belongs to another purpose kind. *)
  val verify_by_challenge
    :  t
    -> challenge_id:string
    -> purpose:Otp.Purpose.t
    -> code:string
    -> (string option * Otp.Attempt.t) Deferred.t
end
