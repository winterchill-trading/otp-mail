(** A ready-made email that delivers a one-time code, in English, German, French, Spanish,
    Italian, Portuguese (European), Russian and Kyrgyz. The Russian and Kyrgyz wording
    awaits a native speaker's review and is marked so in the template
    ({!Template.review_pending}).

    The code is never put in the subject, where lock screens and notification previews
    would show it. A payment confirmation names the amount and the payee, as PSD2 RTS
    article 5(1)(a) requires the payer to be shown what the code authorises. *)

open! Core

type purpose =
  | Sign_in
  | Passcode_reset
  | Payment of
      { amount : string (** formatted for the reader, e.g. ["EUR 25.00"] *)
      ; payee : string (** e.g. ["Acme Ltd, DE89 **** 3000"] *)
      }
[@@deriving sexp_of]

type params =
  { code : string
  ; purpose : purpose
  ; expires_in : Time_ns.Span.t (** shown in whole minutes, rounded up *)
  ; service_name : string (** who is sending, e.g. ["Example Bank"] *)
  }
[@@deriving sexp_of]

(** The template; check it with {!Template.Catalogue.check} alongside your own. *)
val template : params Template.t
