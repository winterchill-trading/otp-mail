open! Core
open! Async

module Received = struct
  type t =
    { delivery_id : Mailer.Delivery_id.t
    ; envelope_from : Address.t
    ; recipients : Address.t list
    ; wire : string
    ; message : Message.t
    ; received_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp_of]
end

type t =
  { time_source : Time_source.t
  ; rng : Rng.t
  ; domain : string
  ; dkim : Dkim.Config.t option
  ; outcome : Address.t -> Mailer.Status.t list
  ; feed : Mailer.Event_feed.t
  ; idempotency : Mailer.Idempotency.t
  ; received : Received.t Queue.t
  ; mutable waiters : (Address.t * Received.t Ivar.t) list
  }

let create
  ?time_source
  ?rng
  ?(domain = "simulated.invalid")
  ?dkim
  ?(outcome = fun _ -> [ Mailer.Status.Delivered ])
  ()
  =
  { time_source =
      (match time_source with
       | None -> Time_source.wall_clock ()
       | Some ts -> Time_source.read_only ts)
  ; rng = Option.value_or_thunk rng ~default:Rng.system
  ; domain
  ; dkim
  ; outcome
  ; feed = Mailer.Event_feed.create ()
  ; idempotency = Mailer.Idempotency.create ()
  ; received = Queue.create ()
  ; waiters = []
  }
;;

let events t = Mailer.Event_feed.subscribe t.feed

let deliver t (message : Message.t) =
  let now = Time_source.now t.time_source in
  let message_id = Message_id.generate ~rng:t.rng ~domain:t.domain in
  let delivery_id =
    Mailer.Delivery_id.of_string (Message_id.local message_id) |> Or_error.ok_exn
  in
  let recipients = Message.recipients message in
  let wire = Message.render message ~date:now ~message_id in
  let signed =
    match t.dkim with
    | None -> Ok wire
    | Some config -> Dkim.sign config ~now wire
  in
  match recipients, Result.bind signed ~f:Message.of_wire with
  | [], _ ->
    Error
      (Mailer.Send_error.Invalid_message (Error.of_string "message has no recipients"))
  | _, Error e -> Error (Mailer.Send_error.Invalid_message e)
  | _, Ok parsed ->
    let received =
      { Received.delivery_id
      ; envelope_from = message.from.address
      ; recipients
      ; wire = Or_error.ok_exn signed
      ; message = parsed
      ; received_at = now
      }
    in
    Queue.enqueue t.received received;
    let publish ?recipient status =
      Mailer.Event_feed.publish t.feed { delivery_id; recipient; status; at = now }
    in
    publish Sent;
    List.iter recipients ~f:(fun recipient ->
      List.iter (t.outcome recipient) ~f:(publish ~recipient));
    let ready, waiting =
      List.partition_tf t.waiters ~f:(fun (address, _) ->
        List.mem recipients address ~equal:Address.equal)
    in
    t.waiters <- waiting;
    List.iter ready ~f:(fun (_, ivar) -> Ivar.fill_if_empty ivar received);
    Ok delivery_id
;;

let send t ?idempotency_key message =
  Mailer.Idempotency.run t.idempotency ~key:idempotency_key message ~send:(fun () ->
    return (deliver t message))
;;

let mailer t =
  Mailer.pack
    (module struct
      type nonrec t = t

      let send = send
      let events = events
    end)
    t
;;

let messages ?to_ ?subject_contains t =
  Queue.to_list t.received
  |> List.filter ~f:(fun (r : Received.t) ->
    (match to_ with
     | None -> true
     | Some a -> List.mem r.recipients a ~equal:Address.equal)
    &&
    match subject_contains with
    | None -> true
    | Some s -> String.is_substring r.message.subject ~substring:s)
;;

let latest t ~to_ = List.last (messages t ~to_)

let next t ~to_ =
  let ivar = Ivar.create () in
  t.waiters <- t.waiters @ [ to_, ivar ];
  Ivar.read ivar
;;

let find t id =
  Queue.find t.received ~f:(fun r -> Mailer.Delivery_id.equal r.delivery_id id)
;;

let count t = Queue.length t.received
let clear t = Queue.clear t.received
