open! Core

module Purpose = struct
  module Kind = struct
    module Stable = struct
      module V1 = struct
        type t =
          | Sign_in
          | Passcode_reset
          | Sca
        [@@deriving sexp, bin_io, compare, equal, hash, enumerate, stable_witness]
      end
    end

    include Stable.V1

    let to_string = function
      | Sign_in -> "sign-in"
      | Passcode_reset -> "passcode-reset"
      | Sca -> "sca"
    ;;
  end

  module Stable = struct
    module V1 = struct
      type t =
        | Sign_in
        | Passcode_reset
        | Sca of { binding : string }
      [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
    end
  end

  include Stable.V1

  let kind : t -> Kind.t = function
    | Sign_in -> Sign_in
    | Passcode_reset -> Passcode_reset
    | Sca _ -> Sca
  ;;

  (* What the code hash covers, so that a code only verifies for its own purpose. *)
  let hash_input = function
    | Sign_in -> "sign-in"
    | Passcode_reset -> "passcode-reset"
    | Sca { binding } -> "sca:" ^ binding
  ;;
end

module Key = struct
  type t =
    { id : string
    ; secret : string
    }

  let sexp_of_t t = [%sexp { id : string = t.id; secret = "<redacted>" }]
  let id t = t.id

  let create ~id ~secret =
    if String.is_empty id
    then Or_error.error_s [%message "OTP key id is empty"]
    else if String.length secret < 32
    then Or_error.error_s [%message "OTP key secret needs at least 32 bytes"]
    else Ok { id; secret }
  ;;

  let of_env ?(var = "OTP_MAIL_OTP_KEY") () =
    match Sys.getenv var with
    | None | Some "" -> Or_error.error_s [%message "environment variable is not set" var]
    | Some hex ->
      (match
         Or_error.try_with (fun () ->
           Cryptokit.transform_string (Cryptokit.Hexa.decode ()) (String.strip hex))
       with
       | Error _ -> Or_error.error_s [%message "environment variable is not hex" var]
       | Ok secret ->
         let id = Option.value (Sys.getenv (var ^ "_ID")) ~default:"k1" in
         create ~id ~secret)
  ;;
end

module Code = struct
  type t = string

  let sexp_of_t _ = Sexp.Atom "<redacted>"
  let to_string t = t
end

module Config = struct
  type t =
    { digits : int
    ; ttl : Time_ns.Span.t
    ; max_attempts : int
    ; lockout_base : Time_ns.Span.t
    ; lockout_max : Time_ns.Span.t
    ; resend_interval : Time_ns.Span.t
    }
  [@@deriving sexp_of, compare, equal]

  let create
    ?(digits = 6)
    ?(ttl = Time_ns.Span.of_min 10.)
    ?(max_attempts = 5)
    ?(lockout_base = Time_ns.Span.minute)
    ?(lockout_max = Time_ns.Span.hour)
    ?(resend_interval = Time_ns.Span.of_sec 30.)
    ()
    =
    let positive name span =
      if Time_ns.Span.(span > zero)
      then Ok ()
      else Or_error.error_s [%message "must be positive" name (span : Time_ns.Span.t)]
    in
    let open Or_error.Let_syntax in
    let%bind () =
      if digits < 4 || digits > 10
      then Or_error.error_s [%message "digits must be between 4 and 10" (digits : int)]
      else Ok ()
    in
    let%bind () =
      if max_attempts < 1 || max_attempts > 10
      then
        Or_error.error_s
          [%message "max_attempts must be between 1 and 10" (max_attempts : int)]
      else Ok ()
    in
    let%bind () = positive "ttl" ttl in
    let%bind () = positive "lockout_base" lockout_base in
    let%bind () = positive "lockout_max" lockout_max in
    let%map () = positive "resend_interval" resend_interval in
    { digits; ttl; max_attempts; lockout_base; lockout_max; resend_interval }
  ;;

  let default = Or_error.ok_exn (create ())

  let lockout_duration t ~lockouts =
    (* The exponent is clamped: 2^20 lockout bases is already far above any sensible
       maximum, and [Span.scale] overflows long before 2^63. *)
    let factor = Float.of_int (1 lsl Int.clamp_exn lockouts ~min:0 ~max:20) in
    Time_ns.Span.min t.lockout_max (Time_ns.Span.scale t.lockout_base factor)
  ;;
end

module Challenge = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : string
        ; key_id : string
        ; code_hash : string
        ; purpose : Purpose.Stable.V1.t
        ; issued_at : Time_ns.Stable.Alternate_sexp.V1.t
        ; expires_at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end

  include Stable.V1
end

module Record = struct
  module Stable = struct
    module V1 = struct
      type t =
        { subject : string
        ; kind : Purpose.Kind.Stable.V1.t
        ; version : int
        ; challenge : Challenge.Stable.V1.t option
        ; failures : int
        ; lockouts : int
        ; locked_until : Time_ns.Stable.Alternate_sexp.V1.t option
        ; last_issued_at : Time_ns.Stable.Alternate_sexp.V1.t option
        }
      [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end

  include Stable.V1

  let empty ~subject ~kind =
    { subject
    ; kind
    ; version = 0
    ; challenge = None
    ; failures = 0
    ; lockouts = 0
    ; locked_until = None
    ; last_issued_at = None
    }
  ;;
end

module Issue_error = struct
  type t =
    | Locked of { until : Time_ns.Alternate_sexp.t }
    | Too_soon of { retry_at : Time_ns.Alternate_sexp.t }
  [@@deriving sexp, compare, equal]
end

module Issued = struct
  type t =
    { challenge_id : string
    ; code : Code.t
    ; expires_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp_of]
end

module Rejection = struct
  type t =
    | No_challenge
    | Expired
    | Wrong_code
    | Locked_out of { until : Time_ns.Alternate_sexp.t }
    | Locked of { until : Time_ns.Alternate_sexp.t }
  [@@deriving sexp, compare, equal]
end

module Verify_result = struct
  type t =
    | Verified
    | Rejected of Rejection.t
  [@@deriving sexp, compare, equal]
end

let hash_code (key : Key.t) ~challenge_id ~purpose ~code =
  let mac = Cryptokit.MAC.hmac_sha256 key.secret in
  Cryptokit.hash_string
    mac
    (String.concat ~sep:"\000" [ challenge_id; Purpose.hash_input purpose; code ])
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
;;

(* Compares every byte whatever the first difference, so the time taken says nothing about
   how much of a guess was right. *)
let constant_time_equal a b =
  String.length a = String.length b
  &&
  let diff = ref 0 in
  for i = 0 to String.length a - 1 do
    diff := !diff lor (Char.to_int a.[i] lxor Char.to_int b.[i])
  done;
  !diff = 0
;;

let lock_active (record : Record.t) ~now =
  match record.locked_until with
  | Some until when Time_ns.( < ) now until -> Some until
  | _ -> None
;;

let clear_expired_lock (record : Record.t) ~now =
  match lock_active record ~now with
  | Some _ -> record
  | None -> { record with locked_until = None }
;;

let issue (config : Config.t) ~key ~rng ~now ~purpose (record : Record.t) =
  match lock_active record ~now with
  | Some until -> Error (Issue_error.Locked { until })
  | None ->
    let too_soon =
      Option.bind record.last_issued_at ~f:(fun last ->
        let retry_at = Time_ns.add last config.resend_interval in
        Option.some_if (Time_ns.( < ) now retry_at) retry_at)
    in
    (match too_soon with
     | Some retry_at -> Error (Issue_error.Too_soon { retry_at })
     | None ->
       let challenge_id = Rng.hex rng 16 in
       let code = Rng.digits rng config.digits in
       let expires_at = Time_ns.add now config.ttl in
       let challenge =
         { Challenge.id = challenge_id
         ; key_id = Key.id key
         ; code_hash = hash_code key ~challenge_id ~purpose ~code
         ; purpose
         ; issued_at = now
         ; expires_at
         }
       in
       let record =
         { (clear_expired_lock record ~now) with
           challenge = Some challenge
         ; last_issued_at = Some now
         }
       in
       Ok (record, { Issued.challenge_id; code; expires_at }))
;;

let normalise_code code =
  String.filter code ~f:(fun c -> not (Char.equal c ' ' || Char.equal c '-'))
;;

let verify (config : Config.t) ~keys ~now ~purpose ~code (record : Record.t) =
  match lock_active record ~now with
  | Some until -> record, Verify_result.Rejected (Locked { until })
  | None ->
    let record = clear_expired_lock record ~now in
    (match record.challenge with
     | None -> record, Verify_result.Rejected No_challenge
     | Some challenge when Time_ns.( >= ) now challenge.expires_at ->
       record, Rejected Expired
     | Some challenge ->
       (match List.find keys ~f:(fun k -> String.equal (Key.id k) challenge.key_id) with
        | None ->
          (* The key was retired, so no code issued under it can be checked. *)
          { record with challenge = None }, Rejected Expired
        | Some key ->
          let expected = challenge.code_hash in
          (* The hash covers the purpose presented now, so a code issued for another
             purpose or SCA binding fails like a wrong code. *)
          let actual =
            hash_code key ~challenge_id:challenge.id ~purpose ~code:(normalise_code code)
          in
          if constant_time_equal expected actual
          then
            ( { record with
                challenge = None
              ; failures = 0
              ; lockouts = 0
              ; locked_until = None
              }
            , Verified )
          else (
            let failures = record.failures + 1 in
            if failures >= config.max_attempts
            then (
              let until =
                Time_ns.add now (Config.lockout_duration config ~lockouts:record.lockouts)
              in
              ( { record with
                  challenge = None
                ; failures = 0
                ; lockouts = record.lockouts + 1
                ; locked_until = Some until
                }
              , Rejected (Locked_out { until }) ))
            else { record with failures }, Rejected Wrong_code)))
;;
