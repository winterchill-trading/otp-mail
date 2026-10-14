(** Where one-time code records live.

    A service keeps records in its own database by implementing {!S}; {!Memory} serves
    tests, demos and single-process tools. {!check} runs the same scenarios against any
    implementation, so a database-backed store can show it behaves like {!Memory}.

    Writes use optimistic concurrency: {!S.save} succeeds only if the stored version is
    the one the caller read. Two concurrent verifications of the same subject therefore
    cannot both count from the same failure total; the loser reloads and tries again
    ({!Otp_service} does this). *)

open! Core
open! Async

module type S = sig
  type t

  (** [load t ~subject ~kind] is the stored record, if any. *)
  val load
    :  t
    -> subject:string
    -> kind:Otp.Purpose.Kind.t
    -> Otp.Record.t option Deferred.t

  (** [save t record ~expected_version] stores [record] if the stored record for its
      subject and kind has version [expected_version] (0 meaning none is stored) and
      [record.version = expected_version + 1]; otherwise it stores nothing and answers
      [`Conflict]. *)
  val save
    :  t
    -> Otp.Record.t
    -> expected_version:int
    -> [ `Saved | `Conflict ] Deferred.t
end

(** A store that can also find a record by its outstanding challenge's id, for clients
    that send back the challenge id and the code without naming the subject. A database
    store keeps an index on the challenge id (unique while outstanding). *)
module type Indexed = sig
  include S

  (** [find_challenge t ~challenge_id] is the subject and kind of the record whose
      outstanding challenge has id [challenge_id]. A challenge that was replaced, used or
      cleared is not found. *)
  val find_challenge
    :  t
    -> challenge_id:string
    -> (string * Otp.Purpose.Kind.t) option Deferred.t
end

module Memory : sig
  include Indexed

  (** [create ()] is an empty store. *)
  val create : unit -> t

  (** [records t] is every stored record, sorted by subject then kind. *)
  val records : t -> Otp.Record.t list
end

(** [check (module Store) store] saves, reloads and conflicts records under subjects
    starting with ["otp-store-check-"] and returns the first difference from the contract
    above. [store] must not hold records for those subjects. *)
val check : (module S with type t = 'a) -> 'a -> unit Or_error.t Deferred.t

(** [check_indexed (module Store) store] runs {!check}, then saves records under subjects
    starting with ["otp-store-check-"] and checks that {!Indexed.find_challenge} follows
    the outstanding challenge: found once saved, gone once replaced or cleared. *)
val check_indexed : (module Indexed with type t = 'a) -> 'a -> unit Or_error.t Deferred.t
