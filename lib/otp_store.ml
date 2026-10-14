open! Core
open! Async

module type S = sig
  type t

  val load
    :  t
    -> subject:string
    -> kind:Otp.Purpose.Kind.t
    -> Otp.Record.t option Deferred.t

  val save
    :  t
    -> Otp.Record.t
    -> expected_version:int
    -> [ `Saved | `Conflict ] Deferred.t
end

module Key = struct
  type t = string * Otp.Purpose.Kind.t [@@deriving compare, hash, sexp_of]
end

module Memory = struct
  type t = (Key.t, Otp.Record.t) Hashtbl.t

  let create () = Hashtbl.create (module Key)
  let load t ~subject ~kind = return (Hashtbl.find t (subject, kind))

  let save t (record : Otp.Record.t) ~expected_version =
    let key = record.subject, record.kind in
    let stored_version =
      Option.value_map (Hashtbl.find t key) ~default:0 ~f:(fun r -> r.Otp.Record.version)
    in
    if stored_version = expected_version && record.version = expected_version + 1
    then (
      Hashtbl.set t ~key ~data:record;
      return `Saved)
    else return `Conflict
  ;;

  let records t =
    Hashtbl.data t
    |> List.sort ~compare:(fun (a : Otp.Record.t) (b : Otp.Record.t) ->
      [%compare: string * Otp.Purpose.Kind.t] (a.subject, a.kind) (b.subject, b.kind))
  ;;
end

let check (type a) (module Store : S with type t = a) (store : a) =
  let open Deferred.Or_error.Let_syntax in
  let expect label ok =
    if ok
    then return ()
    else Deferred.Or_error.error_s [%message "store check failed" label]
  in
  let subject = "otp-store-check-1" in
  let record = Otp.Record.empty ~subject ~kind:Sca in
  let at = Time_ns.of_string_with_utc_offset "2026-01-01 00:00:00Z" in
  let%bind () =
    let%bind.Deferred loaded = Store.load store ~subject ~kind:Sca in
    expect "a new subject has no record" (Option.is_none loaded)
  in
  let v1 = { record with version = 1; failures = 2; last_issued_at = Some at } in
  let%bind () =
    let%bind.Deferred r = Store.save store v1 ~expected_version:0 in
    expect "the first save succeeds" (Poly.equal r `Saved)
  in
  let%bind () =
    let%bind.Deferred loaded = Store.load store ~subject ~kind:Sca in
    expect
      "a saved record loads back unchanged"
      ([%equal: Otp.Record.t option] loaded (Some v1))
  in
  let%bind () =
    let%bind.Deferred r = Store.save store { v1 with failures = 9 } ~expected_version:0 in
    expect "a second first save conflicts" (Poly.equal r `Conflict)
  in
  let%bind () =
    let%bind.Deferred r = Store.save store { v1 with version = 3 } ~expected_version:1 in
    expect "a version that skips ahead conflicts" (Poly.equal r `Conflict)
  in
  let v2 = { v1 with version = 2; failures = 0; lockouts = 1 } in
  let%bind () =
    let%bind.Deferred r = Store.save store v2 ~expected_version:1 in
    expect "the next version saves" (Poly.equal r `Saved)
  in
  let%bind () =
    let%bind.Deferred r = Store.save store { v1 with version = 2 } ~expected_version:1 in
    expect "a stale writer conflicts" (Poly.equal r `Conflict)
  in
  let%bind () =
    let%bind.Deferred loaded = Store.load store ~subject ~kind:Sca in
    expect
      "the stale write left nothing behind"
      ([%equal: Otp.Record.t option] loaded (Some v2))
  in
  let%bind () =
    let%bind.Deferred loaded = Store.load store ~subject ~kind:Sign_in in
    expect "kinds are kept apart" (Option.is_none loaded)
  in
  let%bind () =
    let%bind.Deferred loaded = Store.load store ~subject:"otp-store-check-2" ~kind:Sca in
    expect "subjects are kept apart" (Option.is_none loaded)
  in
  return ()
;;
