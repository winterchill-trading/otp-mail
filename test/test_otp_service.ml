open! Core
open! Async
open Otp_mail
module Service = Otp_service.Make (Otp_store.Memory)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn

let setup () =
  let store = Otp_store.Memory.create () in
  let time_source = Time_source.create ~now:t0 () in
  let service =
    Service.create ~rng:(Rng.of_seed "service-test-seed-01") ~time_source ~key store
  in
  store, time_source, service
;;

let%expect_test "the memory store meets the store contract" =
  let%bind r = Otp_store.check (module Otp_store.Memory) (Otp_store.Memory.create ()) in
  print_s [%sexp (r : unit Or_error.t)];
  [%expect {| (Ok ()) |}];
  return ()
;;

let%expect_test "issue, verify, and the stored record" =
  let store, time_source, service = setup () in
  let%bind issued = Service.issue service ~subject:"c-1" ~purpose:Sign_in in
  let code =
    match issued with
    | Ok (issued : Otp.Issued.t) -> Otp.Code.to_string issued.code
    | Error e -> raise_s [%sexp (e : Otp.Issue_error.t)]
  in
  Time_source.advance_directly time_source ~to_:(Time_ns.add t0 (Time_ns.Span.of_sec 20.));
  let%bind r = Service.verify service ~subject:"c-1" ~purpose:Sign_in ~code in
  print_s [%sexp (r : Otp.Verify_result.t)];
  print_s [%sexp (Otp_store.Memory.records store : Otp.Record.t list)];
  [%expect
    {|
    Verified
    (((subject c-1) (kind Sign_in) (version 2) (challenge ()) (failures 0)
      (lockouts 0) (locked_until ()) (last_issued_at ("2026-10-14 09:00:00Z"))))
    |}];
  return ()
;;

let%expect_test "concurrent wrong guesses are all counted" =
  let store, _, service = setup () in
  let%bind (_ : (Otp.Issued.t, Otp.Issue_error.t) Result.t) =
    Service.issue service ~subject:"c-1" ~purpose:Sign_in
  in
  let%bind results =
    Deferred.List.init ~how:`Parallel 6 ~f:(fun _ ->
      Service.verify service ~subject:"c-1" ~purpose:Sign_in ~code:"not-it")
  in
  print_s
    [%sexp
      (List.sort results ~compare:Otp.Verify_result.compare : Otp.Verify_result.t list)];
  print_s
    [%sexp
      (List.map (Otp_store.Memory.records store) ~f:(fun (r : Otp.Record.t) ->
         r.version, r.lockouts)
       : (int * int) list)];
  [%expect
    {|
    ((Rejected Wrong_code) (Rejected Wrong_code) (Rejected Wrong_code)
     (Rejected Wrong_code) (Rejected (Locked_out (until "2026-10-14 09:01:00Z")))
     (Rejected (Locked (until "2026-10-14 09:01:00Z"))))
    ((6 1))
    |}];
  return ()
;;

(* A store that answers every save with a conflict, as a broken implementation might. *)
module Always_conflicts = struct
  include Otp_store.Memory

  let save _ _ ~expected_version:_ = return `Conflict
end

let%expect_test "a store that always conflicts is reported, not retried forever" =
  let module Broken = Otp_service.Make (Always_conflicts) in
  let service = Broken.create ~key (Otp_store.Memory.create ()) in
  let%bind r =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      Broken.issue service ~subject:"c-1" ~purpose:Sign_in)
  in
  print_s [%sexp (Result.map r ~f:(fun _ -> ()) : unit Or_error.t)];
  [%expect {| (Error ("Otp_service: the store kept reporting conflicts" c-1 (n 20))) |}];
  let%bind r = Otp_store.check (module Always_conflicts) (Otp_store.Memory.create ()) in
  print_s [%sexp (r : unit Or_error.t)];
  [%expect {| (Error ("store check failed" "the first save succeeds")) |}];
  return ()
;;
