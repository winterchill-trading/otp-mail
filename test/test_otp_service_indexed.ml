open! Core
open! Async
open Otp_mail
module Service = Otp_service.Make_indexed (Otp_store.Memory)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn

let%expect_test "the memory store meets the indexed store contract" =
  let%bind r =
    Otp_store.check_indexed (module Otp_store.Memory) (Otp_store.Memory.create ())
  in
  print_s [%sexp (r : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  return ()
;;

let%expect_test "verify by challenge id alone" =
  let store = Otp_store.Memory.create () in
  let config =
    Otp.Config.create ~resend_interval:Time_ns.Span.zero () |> Or_error.ok_exn
  in
  let service =
    Service.create
      ~config
      ~rng:(Rng.of_seed "service-test-seed-02")
      ~time_source:(Time_source.create ~now:t0 ())
      ~key
      store
  in
  let issue subject =
    match%map Service.issue service ~subject ~purpose:Sign_in with
    | Ok (issued : Otp.Issued.t) -> issued.challenge_id, Otp.Code.to_string issued.code
    | Error e -> raise_s [%sexp (e : Otp.Issue_error.t)]
  in
  let%bind id, code = issue "c-1" in
  let%bind other_id, _ = issue "c-2" in
  let show (subject, attempt) =
    print_s [%sexp (subject : string option), (attempt : Otp.Attempt.t)]
  in
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Sign_in ~code:"000000"
    >>| show
  in
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:"unknown" ~purpose:Sign_in ~code
    >>| show
  in
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Passcode_reset ~code
    >>| show
  in
  (* Another subject's challenge id does not open this subject's code. *)
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:other_id ~purpose:Sign_in ~code
    >>| show
  in
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Sign_in ~code >>| show
  in
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Sign_in ~code >>| show
  in
  (* A re-sent code makes the earlier id unknown. *)
  let%bind old_id, _ = issue "c-1" in
  let%bind new_id, new_code = issue "c-1" in
  let%bind () =
    Service.verify_challenge
      service
      ~subject:"c-1"
      ~challenge_id:old_id
      ~purpose:Sign_in
      ~code:new_code
    >>| fun a -> print_s [%sexp (a : Otp.Attempt.t)]
  in
  let%bind () =
    Service.verify_challenge
      service
      ~subject:"c-1"
      ~challenge_id:new_id
      ~purpose:Sign_in
      ~code:new_code
    >>| fun a -> print_s [%sexp (a : Otp.Attempt.t)]
  in
  [%expect
    {|
    ((c-1) (Wrong_code (attempts_left 4)))
    (() No_challenge)
    (() No_challenge)
    ((c-2) (Wrong_code (attempts_left 4)))
    ((c-1) Verified)
    (() No_challenge)
    No_challenge
    Verified
    |}];
  return ()
;;
