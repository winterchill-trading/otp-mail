open! Core
open Otp_mail

let key = Otp.Key.create ~id:"k1" ~secret:(String.make 32 'k') |> Or_error.ok_exn
let config = Otp.Config.default
let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

let%test_unit "codes have the configured length and are digits" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       (Int.gen_incl 4 10)
       (String.gen_with_length 16 Char.gen_print))
    ~sexp_of:[%sexp_of: int * string]
    ~f:(fun (digits, seed) ->
      let code = Rng.digits (Rng.of_seed seed) digits in
      assert (String.length code = digits);
      assert (String.for_all code ~f:Char.is_digit))
;;

let%expect_test "digits are uniform (chi-squared over 200 000 draws)" =
  let counts = Array.create ~len:10 0 in
  let draws = 200_000 in
  String.iter
    (Rng.digits (Rng.of_seed "uniformity-check-seed") draws)
    ~f:(fun c ->
      let d = Char.get_digit_exn c in
      counts.(d) <- counts.(d) + 1);
  let expected = Float.of_int draws /. 10. in
  let chi2 =
    Array.fold counts ~init:0. ~f:(fun acc n ->
      let d = Float.of_int n -. expected in
      acc +. (d *. d /. expected))
  in
  (* 21.67 is the 99% quantile of chi-squared with 9 degrees of freedom. *)
  print_s [%sexp (Float.(chi2 < 21.67) : bool)];
  [%expect {| true |}]
;;

module Op = struct
  type t =
    | Issue of [ `A | `B ]
    | Verify_right of [ `A | `B | `Sign_in ]
    | Verify_wrong
    | Wait of int
  [@@deriving sexp_of, quickcheck]

  (* Waits and wrong guesses are common enough to reach lockouts and expiry. *)
  let quickcheck_generator =
    Quickcheck.Generator.weighted_union
      [ ( 2.
        , Quickcheck.Generator.map
            (Quickcheck.Generator.of_list [ `A; `B ])
            ~f:(fun b -> Issue b) )
      ; ( 3.
        , Quickcheck.Generator.map
            (Quickcheck.Generator.of_list [ `A; `B; `Sign_in ])
            ~f:(fun b -> Verify_right b) )
      ; 4., Quickcheck.Generator.return Verify_wrong
      ; 2., Quickcheck.Generator.map (Int.gen_incl 1 900) ~f:(fun s -> Wait s)
      ]
  ;;
end

let purpose = function
  | `A -> Otp.Purpose.Sca { binding = "EUR 25.00 to payee A" }
  | `B -> Otp.Purpose.Sca { binding = "EUR 25.00 to payee B" }
  | `Sign_in -> Otp.Purpose.Sign_in
;;

(* Checks observable rules against a sequence of operations on one record, tracking only
   what a caller could know: the codes it was given and the results it saw. *)
let run ops =
  let rng = Rng.of_seed "property-run-seed-0001" in
  let record = ref (Otp.Record.empty ~subject:"s" ~kind:Sca) in
  let now = ref t0 in
  let last : (string * Otp.Purpose.t * Time_ns.t) option ref = ref None in
  let used = ref false in
  let wrong_in_a_row = ref 0 in
  let locked_until = ref None in
  let locked () =
    match !locked_until with
    | Some u -> Time_ns.( < ) !now u
    | None -> false
  in
  List.iter ops ~f:(fun (op : Op.t) ->
    (match op with
     | Wait s -> now := Time_ns.add !now (Time_ns.Span.of_int_sec s)
     | Issue b ->
       (match Otp.issue config ~key ~rng ~now:!now ~purpose:(purpose b) !record with
        | Ok (r, issued) ->
          assert (not (locked ()));
          record := r;
          last := Some (Otp.Code.to_string issued.code, purpose b, !now);
          used := false
        | Error (Locked _) -> assert (locked ())
        | Error (Too_soon _) -> ())
     | Verify_right b ->
       (match !last with
        | None -> ()
        | Some (code, issued_for, issued_at) ->
          let p = purpose b in
          let r, result =
            Otp.verify config ~keys:[ key ] ~now:!now ~purpose:p ~code !record
          in
          record := r;
          let fresh = Time_ns.( < ) !now (Time_ns.add issued_at config.ttl) in
          let should_verify =
            fresh && (not !used) && Otp.Purpose.equal p issued_for && not (locked ())
          in
          (match result with
           | Verified ->
             assert should_verify;
             used := true;
             wrong_in_a_row := 0
           | Rejected (Locked_out { until }) ->
             assert (not should_verify);
             locked_until := Some until;
             used := true;
             wrong_in_a_row := 0
           | Rejected Wrong_code ->
             assert (not should_verify);
             incr wrong_in_a_row
           | Rejected (Locked _) -> assert (locked ())
           | Rejected (No_challenge | Expired) -> assert (not should_verify)))
     | Verify_wrong ->
       let r, result =
         Otp.verify
           config
           ~keys:[ key ]
           ~now:!now
           ~purpose:(purpose `A)
           ~code:"12345x"
           !record
       in
       record := r;
       (match result with
        | Verified -> assert false
        | Rejected Wrong_code -> incr wrong_in_a_row
        | Rejected (Locked_out { until }) ->
          locked_until := Some until;
          used := true;
          wrong_in_a_row := 0
        | Rejected (Locked _) -> assert (locked ())
        | Rejected (No_challenge | Expired) -> ()));
    assert (!wrong_in_a_row < config.max_attempts))
;;

let%test_unit "observable rules hold over random sessions" =
  Quickcheck.test
    (List.gen_with_length 40 Op.quickcheck_generator)
    ~sexp_of:[%sexp_of: Op.t list]
    ~trials:2_000
    ~f:run
;;

let%test_unit "records never contain the code" =
  Quickcheck.test
    (String.gen_with_length 16 Char.gen_print)
    ~sexp_of:[%sexp_of: string]
    ~f:(fun seed ->
      match
        Otp.issue
          (Otp.Config.create ~digits:10 () |> Or_error.ok_exn)
          ~key
          ~rng:(Rng.of_seed seed)
          ~now:t0
          ~purpose:Sign_in
          (Otp.Record.empty ~subject:"s" ~kind:Sign_in)
      with
      | Error _ -> assert false
      | Ok (record, issued) ->
        let stored = Sexp.to_string [%sexp (record : Otp.Record.t)] in
        let bin = Binable.to_string (module Otp.Record.Stable.V1) record in
        let code = Otp.Code.to_string issued.code in
        assert (not (String.is_substring stored ~substring:code));
        assert (not (String.is_substring bin ~substring:code)))
;;
