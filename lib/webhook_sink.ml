open! Core

module type S = sig
  type t

  val parse
    :  t
    -> headers:(string * string) list
    -> body:string
    -> Mailer.Event.t list Or_error.t
end

let hmac_sha256_hex ~key data =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) data
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
;;

let verify_hmac_sha256 ~key ~body ~signature =
  let expected = hmac_sha256_hex ~key body in
  let actual = String.lowercase (String.strip signature) in
  String.length expected = String.length actual
  &&
  let diff = ref 0 in
  for i = 0 to String.length expected - 1 do
    diff := !diff lor (Char.to_int expected.[i] lxor Char.to_int actual.[i])
  done;
  !diff = 0
;;

module Sink = struct
  module Seen = struct
    type t = Mailer.Event.t [@@deriving compare, sexp_of]

    let hash t = String.hash (Sexp.to_string (sexp_of_t t))
  end

  type t =
    { feed : Mailer.Event_feed.t
    ; capacity : int
    ; seen : (Seen.t, unit) Hashtbl.t
    ; order : Seen.t Queue.t
    }

  let create ?(capacity = 100_000) feed =
    { feed; capacity; seen = Hashtbl.create (module Seen); order = Queue.create () }
  ;;

  let receive (type a) t (module P : S with type t = a) (p : a) ~headers ~body =
    Or_error.map (P.parse p ~headers ~body) ~f:(fun events ->
      List.count events ~f:(fun event ->
        if Hashtbl.mem t.seen event
        then false
        else (
          Hashtbl.set t.seen ~key:event ~data:();
          Queue.enqueue t.order event;
          while Queue.length t.order > t.capacity do
            Hashtbl.remove t.seen (Queue.dequeue_exn t.order)
          done;
          Mailer.Event_feed.publish t.feed event;
          true)))
  ;;
end
