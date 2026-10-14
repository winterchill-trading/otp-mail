open! Core
open Otp_mail

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let at seconds = Time_ns.add t0 (Time_ns.Span.of_sec seconds)
let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn
let key2 = Otp.Key.create ~id:"k2" ~secret:(String.make 32 'q') |> Or_error.ok_exn
let config = Otp.Config.default
let fresh kind = Otp.Record.empty ~subject:"customer-1" ~kind

let issue ?(rng = Rng.of_seed "otp-tests-seed-0001") ?(key = key) ~now ~purpose record =
  Otp.issue config ~key ~rng ~now ~purpose record
;;

let issue_exn ?rng ?key ~now ~purpose record =
  match issue ?rng ?key ~now ~purpose record with
  | Ok (record, issued) -> record, Otp.Code.to_string issued.code
  | Error e -> raise_s [%sexp (e : Otp.Issue_error.t)]
;;

let verify ?(keys = [ key ]) ~now ~purpose ~code record =
  let record, result = Otp.verify config ~keys ~now ~purpose ~code record in
  print_s [%sexp (result : Otp.Verify_result.t)];
  record
;;

let%expect_test "a code is accepted once" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  print_endline code;
  [%expect {| 334647 |}];
  let record = verify ~now:(at 30.) ~purpose:Sign_in ~code record in
  let (_ : Otp.Record.t) = verify ~now:(at 31.) ~purpose:Sign_in ~code record in
  [%expect {|
    Verified
    (Rejected No_challenge)
    |}]
;;

let%expect_test "codes and keys never print" =
  let rng = Rng.of_seed "otp-tests-seed-0001" in
  match Otp.issue config ~key ~rng ~now:t0 ~purpose:Sign_in (fresh Sign_in) with
  | Error _ -> assert false
  | Ok (record, issued) ->
    print_s [%sexp (issued : Otp.Issued.t)];
    print_s [%sexp (key : Otp.Key.t)];
    print_s [%sexp (record : Otp.Record.t)];
    [%expect
      {|
      ((challenge_id 6527015e053b57c7099a0589c63688a4) (code <redacted>)
       (expires_at "2026-10-14 09:10:00Z"))
      ((id k1) (secret <redacted>))
      ((subject customer-1) (kind Sign_in) (version 0)
       (challenge
        (((id 6527015e053b57c7099a0589c63688a4) (key_id k1)
          (code_hash
           a2dd0dbd4f9013562fdaa94116b138fb3a840317946d20c4c098381b46528053)
          (purpose Sign_in) (issued_at "2026-10-14 09:00:00Z")
          (expires_at "2026-10-14 09:10:00Z"))))
       (failures 0) (lockouts 0) (locked_until ())
       (last_issued_at ("2026-10-14 09:00:00Z")))
      |}]
;;

let%expect_test "separators in typed codes are ignored" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  let typed = String.prefix code 3 ^ " - " ^ String.drop_prefix code 3 in
  let (_ : Otp.Record.t) = verify ~now:(at 1.) ~purpose:Sign_in ~code:typed record in
  [%expect {| Verified |}]
;;

let%expect_test "expiry" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  let (_ : Otp.Record.t) = verify ~now:(at 600.) ~purpose:Sign_in ~code record in
  [%expect {| (Rejected Expired) |}]
;;

let%expect_test "a code only works for its purpose and SCA binding" =
  let sca binding = Otp.Purpose.Sca { binding } in
  let record, code =
    issue_exn ~now:t0 ~purpose:(sca "EUR 25.00 to DE89...3000") (fresh Sca)
  in
  let record =
    verify ~now:(at 5.) ~purpose:(sca "EUR 2500.00 to DE89...3000") ~code record
  in
  let record = verify ~now:(at 6.) ~purpose:Sign_in ~code record in
  let record =
    verify ~now:(at 7.) ~purpose:(sca "EUR 25.00 to DE89...3000") ~code record
  in
  print_s [%sexp (record.failures : int)];
  [%expect
    {|
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    Verified
    0
    |}]
;;

let%expect_test "five failures lock the subject out, with doubling lockouts" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  let wrong = if String.equal code "000000" then "111111" else "000000" in
  let record =
    List.fold (List.range 1 6) ~init:record ~f:(fun record i ->
      verify ~now:(at (Float.of_int i)) ~purpose:Sign_in ~code:wrong record)
  in
  [%expect
    {|
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected (Locked_out (until "2026-10-14 09:01:05Z")))
    |}];
  let record = verify ~now:(at 10.) ~purpose:Sign_in ~code record in
  print_s
    [%sexp
      (issue ~now:(at 10.) ~purpose:Sign_in record |> Result.map ~f:(fun _ -> ())
       : (unit, Otp.Issue_error.t) Result.t)];
  [%expect
    {|
    (Rejected (Locked (until "2026-10-14 09:01:05Z")))
    (Error (Locked (until "2026-10-14 09:01:05Z")))
    |}];
  (* After the lock, a new code works, and success clears the lockout count. *)
  let record, code = issue_exn ~now:(at 66.) ~purpose:Sign_in record in
  let record = verify ~now:(at 67.) ~purpose:Sign_in ~code record in
  print_s [%sexp (record.lockouts : int), (record.failures : int)];
  [%expect {|
    Verified
    (0 0)
    |}]
;;

let%expect_test "lockout durations" =
  List.iter (List.range 0 9) ~f:(fun lockouts ->
    printf
      "%d: %s\n"
      lockouts
      (Time_ns.Span.to_string_hum (Otp.Config.lockout_duration config ~lockouts)));
  print_endline
    (Time_ns.Span.to_string_hum (Otp.Config.lockout_duration config ~lockouts:1_000));
  [%expect
    {|
    0: 1m
    1: 2m
    2: 4m
    3: 8m
    4: 16m
    5: 32m
    6: 1h
    7: 1h
    8: 1h
    1h
    |}]
;;

let%expect_test "failures carry over to a re-sent code" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  let wrong = if String.equal code "000000" then "111111" else "000000" in
  let record =
    List.fold (List.range 1 5) ~init:record ~f:(fun record i ->
      verify ~now:(at (Float.of_int i)) ~purpose:Sign_in ~code:wrong record)
  in
  let record, _ =
    issue_exn ~rng:(Rng.of_seed "another-seed-0002") ~now:(at 40.) ~purpose:Sign_in record
  in
  let (_ : Otp.Record.t) = verify ~now:(at 41.) ~purpose:Sign_in ~code:wrong record in
  [%expect
    {|
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected Wrong_code)
    (Rejected (Locked_out (until "2026-10-14 09:01:41Z")))
    |}]
;;

let%expect_test "re-sending too soon" =
  let record, _ = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  print_s
    [%sexp
      (issue ~now:(at 10.) ~purpose:Sign_in record |> Result.map ~f:(fun _ -> ())
       : (unit, Otp.Issue_error.t) Result.t)];
  [%expect {| (Error (Too_soon (retry_at "2026-10-14 09:00:30Z"))) |}]
;;

let%expect_test "key rotation" =
  let record, code = issue_exn ~now:t0 ~purpose:Sign_in (fresh Sign_in) in
  let (_ : Otp.Record.t) =
    verify ~keys:[ key2; key ] ~now:(at 1.) ~purpose:Sign_in ~code record
  in
  let (_ : Otp.Record.t) =
    verify ~keys:[ key2 ] ~now:(at 1.) ~purpose:Sign_in ~code record
  in
  [%expect {|
    Verified
    (Rejected Expired)
    |}];
  print_s [%sexp (Otp.Key.create ~id:"short" ~secret:"too short" : Otp.Key.t Or_error.t)];
  [%expect {| (Error "OTP key secret needs at least 32 bytes") |}]
;;

let%expect_test "config bounds" =
  let show r = print_s [%sexp (Result.map r ~f:(fun _ -> ()) : unit Or_error.t)] in
  show (Otp.Config.create ~digits:3 ());
  show (Otp.Config.create ~digits:11 ());
  show (Otp.Config.create ~max_attempts:0 ());
  show (Otp.Config.create ~ttl:Time_ns.Span.zero ());
  show (Otp.Config.create ~digits:8 ~max_attempts:3 ());
  [%expect
    {|
    (Error ("digits must be between 4 and 10" (digits 3)))
    (Error ("digits must be between 4 and 10" (digits 11)))
    (Error ("max_attempts must be between 1 and 10" (max_attempts 0)))
    (Error ("must be positive" ttl (span 0s)))
    (Ok ())
    |}]
;;

let%expect_test "stable digests" =
  List.iter
    [ Otp.Purpose.Kind.Stable.V1.bin_shape_t |> Bin_prot.Shape.eval_to_digest_string
    ; Otp.Purpose.Stable.V1.bin_shape_t |> Bin_prot.Shape.eval_to_digest_string
    ; Otp.Challenge.Stable.V1.bin_shape_t |> Bin_prot.Shape.eval_to_digest_string
    ; Otp.Record.Stable.V1.bin_shape_t |> Bin_prot.Shape.eval_to_digest_string
    ]
    ~f:print_endline;
  [%expect
    {|
    9abc4339a242f93525ef73d298427196
    181d2ef2dbb1997f607b40501d45362a
    4dda8e201d5d1e461d86bee97359205d
    3774c94fa249498018d1e3d1970556e7
    |}]
;;

let%expect_test "waits can be switched off for tests" =
  let zero = Time_ns.Span.zero in
  let show r = print_s [%sexp (Result.map r ~f:(fun _ -> ()) : unit Or_error.t)] in
  show (Otp.Config.create ~resend_interval:zero ~lockout_base:zero ~lockout_max:zero ());
  show (Otp.Config.create ~resend_interval:(Time_ns.Span.of_sec (-1.)) ());
  [%expect
    {|
    (Ok ())
    (Error ("must not be negative" resend_interval (span -1s)))
    |}];
  let config =
    Otp.Config.create ~resend_interval:zero ~lockout_base:zero ~lockout_max:zero ()
    |> Or_error.ok_exn
  in
  let rng = Rng.of_seed "otp-tests-seed-0001" in
  let issue record =
    match Otp.issue config ~key ~rng ~now:t0 ~purpose:Sign_in record with
    | Ok (record, _) -> record, "ok"
    | Error e -> record, Sexp.to_string [%sexp (e : Otp.Issue_error.t)]
  in
  (* Two codes in the same instant, then five wrong guesses: the lockout ends at once. *)
  let record, first = issue (fresh Sign_in) in
  let record, second = issue record in
  let record =
    List.fold (List.range 0 5) ~init:record ~f:(fun record _ ->
      fst (Otp.verify config ~keys:[ key ] ~now:t0 ~purpose:Sign_in ~code:"000000" record))
  in
  let _, third = issue record in
  print_s [%sexp (first : string), (second : string), (third : string)];
  print_s [%sexp (record.lockouts : int)];
  [%expect {|
    (ok ok ok)
    1
    |}]
;;
