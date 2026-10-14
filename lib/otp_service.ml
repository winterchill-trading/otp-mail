open! Core
open! Async

module Make (Store : Otp_store.S) = struct
  type t =
    { config : Otp.Config.t
    ; rng : Rng.t
    ; time_source : Time_source.t
    ; key : Otp.Key.t
    ; keys : Otp.Key.t list
    ; store : Store.t
    }

  let create
    ?(config = Otp.Config.default)
    ?(rng = Rng.system ())
    ?time_source
    ?(previous_keys = [])
    ~key
    store
    =
    { config
    ; rng
    ; time_source =
        (match time_source with
         | None -> Time_source.wall_clock ()
         | Some ts -> Time_source.read_only ts)
    ; key
    ; keys = key :: previous_keys
    ; store
    }
  ;;

  (* Conflicts mean another request for the same subject saved in between; each retry sees
     its result. A long run of conflicts means a broken store, not contention. *)
  let max_attempts = 20

  let update t ~subject ~(purpose : Otp.Purpose.t) ~f =
    let kind = Otp.Purpose.kind purpose in
    let rec attempt n =
      let%bind stored = Store.load t.store ~subject ~kind in
      let record = Option.value stored ~default:(Otp.Record.empty ~subject ~kind) in
      let now = Time_source.now t.time_source in
      match f record ~now with
      | None, result -> return result
      | Some (next : Otp.Record.t), result ->
        (match%bind
           Store.save
             t.store
             { next with version = record.version + 1 }
             ~expected_version:record.version
         with
         | `Saved -> return result
         | `Conflict ->
           if n >= max_attempts
           then
             raise_s
               [%message
                 "Otp_service: the store kept reporting conflicts" subject (n : int)]
           else attempt (n + 1))
    in
    attempt 1
  ;;

  let issue t ~subject ~purpose =
    update t ~subject ~purpose ~f:(fun record ~now ->
      match Otp.issue t.config ~key:t.key ~rng:t.rng ~now ~purpose record with
      | Ok (next, issued) -> Some next, Ok issued
      | Error e -> None, Error e)
  ;;

  let verify t ~subject ~purpose ~code =
    update t ~subject ~purpose ~f:(fun record ~now ->
      let next, result = Otp.verify t.config ~keys:t.keys ~now ~purpose ~code record in
      (if Otp.Record.equal next record then None else Some next), result)
  ;;

  let verify_challenge t ~subject ~challenge_id ~purpose ~code =
    update t ~subject ~purpose ~f:(fun record ~now ->
      let next, result =
        Otp.verify_challenge
          t.config
          ~keys:t.keys
          ~now
          ~challenge_id
          ~purpose
          ~code
          record
      in
      (if Otp.Record.equal next record then None else Some next), result)
  ;;
end

module Make_indexed (Store : Otp_store.Indexed) = struct
  include Make (Store)

  let store t = t.store

  let verify_by_challenge t ~challenge_id ~(purpose : Otp.Purpose.t) ~code =
    match%bind Store.find_challenge (store t) ~challenge_id with
    | Some (subject, kind) when Otp.Purpose.Kind.equal kind (Otp.Purpose.kind purpose) ->
      let%map attempt = verify_challenge t ~subject ~challenge_id ~purpose ~code in
      Some subject, attempt
    | Some _ | None -> return (None, Otp.Attempt.No_challenge)
  ;;
end
