open! Core
open! Async
open Otp_mail

(* A plain store: the memory store seen only through [Otp_store.S], so nothing can reach
   its own [find_challenge]. *)
module Plain : Otp_store.S with type t = Otp_store.Memory.t = Otp_store.Memory
module Adapted = Otp_store.Indexed (Plain)
module Service = Otp_service.Make_indexed (Adapted)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn

let%expect_test "the adapter meets the indexed store contract" =
  let%bind r =
    Otp_store.check_indexed (module Adapted) (Adapted.create (Otp_store.Memory.create ()))
  in
  print_s [%sexp (r : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  return ()
;;

let service ?(seed = "adapter-test-seed-01") store =
  let config =
    Otp.Config.create ~resend_interval:Time_ns.Span.zero () |> Or_error.ok_exn
  in
  Service.create
    ~config
    ~rng:(Rng.of_seed seed)
    ~time_source:(Time_source.create ~now:t0 ())
    ~key
    store
;;

let issue service subject =
  match%map Service.issue service ~subject ~purpose:Sign_in with
  | Ok (issued : Otp.Issued.t) -> issued.challenge_id, Otp.Code.to_string issued.code
  | Error e -> raise_s [%sexp (e : Otp.Issue_error.t)]
;;

let show (subject, attempt) =
  print_s [%sexp (subject : string option), (attempt : Otp.Attempt.t)]
;;

let%expect_test "verify by challenge id over a plain store" =
  let adapted = Adapted.create (Otp_store.Memory.create ()) in
  let service = service adapted in
  let%bind id, code = issue service "c-1" in
  let%bind _ = issue service "c-2" in
  print_s [%sexp (Adapted.length adapted : int)];
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Sign_in ~code >>| show
  in
  (* Used: the challenge is cleared and its entry dropped. *)
  let%bind () =
    Service.verify_by_challenge service ~challenge_id:id ~purpose:Sign_in ~code >>| show
  in
  print_s [%sexp (Adapted.length adapted : int)];
  [%expect {|
    2
    ((c-1) Verified)
    (() No_challenge)
    1
    |}];
  return ()
;;

let%expect_test "a second process does not see this one's challenges" =
  let shared = Otp_store.Memory.create () in
  let first = service (Adapted.create shared) in
  let%bind id, code = issue first "c-1" in
  (* A fresh adapter over the same records: a restart, or another instance. *)
  let fresh = Adapted.create shared in
  let%bind () =
    Service.verify_by_challenge (service fresh) ~challenge_id:id ~purpose:Sign_in ~code
    >>| show
  in
  (* Loading the subject's record (as verification by subject does) teaches it. *)
  let%bind (_ : Otp.Record.t option) = Adapted.load fresh ~subject:"c-1" ~kind:Sign_in in
  let%bind found = Adapted.find_challenge fresh ~challenge_id:id in
  print_s [%sexp (found : (string * Otp.Purpose.Kind.t) option)];
  [%expect {|
    (() No_challenge)
    ((c-1 Sign_in))
    |}];
  return ()
;;

let%expect_test "a challenge replaced behind the index's back is not reported" =
  let shared = Otp_store.Memory.create () in
  let a = Adapted.create shared in
  let b = service ~seed:"adapter-test-seed-02" (Adapted.create shared) in
  let%bind old_id, _ = issue (service a) "c-1" in
  (* The other instance re-sends; [a]'s index still points at the old id. *)
  let%bind _ = issue b "c-1" in
  let%bind found = Adapted.find_challenge a ~challenge_id:old_id in
  print_s [%sexp (found : (string * Otp.Purpose.Kind.t) option), (Adapted.length a : int)];
  [%expect {| (() 1) |}];
  return ()
;;
