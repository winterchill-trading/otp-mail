open! Core
open Otp_mail

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let at seconds = Time_ns.add t0 (Time_ns.Span.of_sec seconds)
let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn
let config = Otp.Config.default
let fresh () = Otp.Record.empty ~subject:"customer-1" ~kind:Sign_in

let issue ?(rng = Rng.of_seed "otp-challenge-seed-01") ~now record =
  match Otp.issue config ~key ~rng ~now ~purpose:Sign_in record with
  | Ok (record, issued) -> record, issued.challenge_id, Otp.Code.to_string issued.code
  | Error e -> raise_s [%sexp (e : Otp.Issue_error.t)]
;;

let check ~now ~challenge_id ~code record =
  let record, attempt =
    Otp.verify_challenge
      config
      ~keys:[ key ]
      ~now
      ~challenge_id
      ~purpose:Sign_in
      ~code
      record
  in
  print_s [%sexp (attempt : Otp.Attempt.t)];
  record
;;

let%expect_test "attempts count down, and a lockout keeps the challenge" =
  let record, id, code = issue ~now:t0 (fresh ()) in
  print_s [%sexp (Otp.attempts_left config record : int)];
  let record =
    List.fold (List.range 0 5) ~init:record ~f:(fun record i ->
      check ~now:(at (Float.of_int i)) ~challenge_id:id ~code:"000000" record)
  in
  print_s [%sexp (Option.is_some record.challenge : bool)];
  (* While locked, even the right code is refused. *)
  let record = check ~now:(at 30.) ~challenge_id:id ~code record in
  (* After the lockout the same code still works. *)
  let record = check ~now:(at 65.) ~challenge_id:id ~code record in
  print_s [%sexp (record.failures : int), (record.lockouts : int)];
  [%expect
    {|
    5
    (Wrong_code (attempts_left 4))
    (Wrong_code (attempts_left 3))
    (Wrong_code (attempts_left 2))
    (Wrong_code (attempts_left 1))
    (Locked_out (until "2026-10-14 09:01:04Z"))
    true
    (Locked (until "2026-10-14 09:01:04Z"))
    Verified
    (0 0)
    |}]
;;

let%expect_test "an older challenge id is not counted" =
  let record, old_id, old_code = issue ~now:t0 (fresh ()) in
  let record, new_id, code =
    issue ~rng:(Rng.of_seed "otp-challenge-seed-02") ~now:(at 31.) record
  in
  print_s [%sexp (String.equal old_id new_id : bool)];
  let record = check ~now:(at 32.) ~challenge_id:old_id ~code:old_code record in
  let record = check ~now:(at 33.) ~challenge_id:old_id ~code record in
  print_s [%sexp (record.failures : int)];
  let record = check ~now:(at 34.) ~challenge_id:new_id ~code record in
  let (_ : Otp.Record.t) = check ~now:(at 35.) ~challenge_id:new_id ~code record in
  [%expect
    {|
    false
    No_challenge
    No_challenge
    0
    Verified
    No_challenge
    |}]
;;

let%expect_test "failures carry over to a re-sent code" =
  let record, id, _ = issue ~now:t0 (fresh ()) in
  let record = check ~now:(at 1.) ~challenge_id:id ~code:"000000" record in
  let record = check ~now:(at 2.) ~challenge_id:id ~code:"000000" record in
  let record, id, _ =
    issue ~rng:(Rng.of_seed "otp-challenge-seed-02") ~now:(at 31.) record
  in
  print_s [%sexp (Otp.attempts_left config record : int)];
  let (_ : Otp.Record.t) = check ~now:(at 32.) ~challenge_id:id ~code:"000000" record in
  [%expect
    {|
    (Wrong_code (attempts_left 4))
    (Wrong_code (attempts_left 3))
    3
    (Wrong_code (attempts_left 2))
    |}]
;;

let%expect_test "expiry and a lock outlasting the code" =
  let record, id, code = issue ~now:t0 (fresh ()) in
  let (_ : Otp.Record.t) = check ~now:(at 600.) ~challenge_id:id ~code record in
  let record =
    List.fold (List.range 0 5) ~init:record ~f:(fun record _ ->
      fst
        (Otp.verify_challenge
           config
           ~keys:[ key ]
           ~now:(at 590.)
           ~challenge_id:id
           ~purpose:Sign_in
           ~code:"000000"
           record))
  in
  let (_ : Otp.Record.t) = check ~now:(at 620.) ~challenge_id:id ~code record in
  let (_ : Otp.Record.t) = check ~now:(at 700.) ~challenge_id:id ~code record in
  [%expect {|
    Expired
    (Locked (until "2026-10-14 09:10:50Z"))
    Expired
    |}]
;;

let%test_unit "verify_challenge agrees with verify until the first lockout" =
  let gen =
    Quickcheck.Generator.tuple2
      (List.gen_with_length 8 (Quickcheck.Generator.of_list [ `Right; `Wrong; `Wait ]))
      (String.gen_with_length 8 Char.gen_alphanum)
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: [ `Right | `Wrong | `Wait ] list * string]
    ~f:(fun (steps, seed) ->
      let record, id, code =
        issue ~rng:(Rng.of_seed ("otp-challenge-prop-" ^ seed)) ~now:t0 (fresh ())
      in
      (* [`Wait] waits 70 s, then enters a wrong code. After a lockout the two functions
         part ways on purpose (one keeps the challenge), so the comparison stops there. *)
      let rec go a b s = function
        | [] -> ()
        | step :: rest ->
          let s = if Poly.equal step `Wait then s +. 70. else s +. 1. in
          let code = if Poly.equal step `Right then code else "000000" in
          let a, ra =
            Otp.verify config ~keys:[ key ] ~now:(at s) ~purpose:Sign_in ~code a
          in
          let b, rb =
            Otp.verify_challenge
              config
              ~keys:[ key ]
              ~now:(at s)
              ~challenge_id:id
              ~purpose:Sign_in
              ~code
              b
          in
          [%test_result: Otp.Verify_result.t] (Otp.Attempt.to_verify_result rb) ~expect:ra;
          [%test_result: int] b.failures ~expect:a.failures;
          [%test_result: int] b.lockouts ~expect:a.lockouts;
          (match ra with
           | Rejected (Locked_out _) -> ()
           | _ -> go a b s rest)
      in
      go record record 1. steps)
;;
