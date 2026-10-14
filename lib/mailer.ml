open! Core
open! Async

module Delivery_id = struct
  module T = struct
    type t = string [@@deriving compare, equal, hash, sexp_of]
  end

  let of_string s =
    if (not (String.is_empty s))
       && String.for_all s ~f:(fun c -> Char.to_int c > 32 && Char.to_int c < 127)
    then Ok s
    else Or_error.error_s [%message "invalid delivery id" s]
  ;;

  let to_string t = t

  let t_of_sexp sexp =
    match of_string (String.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> of_sexp_error (Error.to_string_hum e) sexp
  ;;

  module Stable = struct
    module V1 = struct
      type t = string [@@deriving compare, equal, hash, sexp_of, stable_witness]

      let t_of_sexp = t_of_sexp

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (String)
          (struct
            type nonrec t = t

            let to_binable t = t
            let of_binable s = Or_error.ok_exn (of_string s)
          end)
    end
  end

  include T

  let t_of_sexp = t_of_sexp

  include Comparable.Make (struct
      include T

      let t_of_sexp = t_of_sexp
    end)

  include Hashable.Make (struct
      include T

      let t_of_sexp = t_of_sexp
    end)
end

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Queued
        | Sent
        | Delivered
        | Deferred of { reason : string }
        | Bounced of { reason : string }
        | Complained
        | Failed of { reason : string }
      [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end

  include Stable.V1

  let is_final = function
    | Delivered | Bounced _ | Complained | Failed _ -> true
    | Queued | Sent | Deferred _ -> false
  ;;
end

module Event = struct
  module Stable = struct
    module V1 = struct
      type t =
        { delivery_id : Delivery_id.Stable.V1.t
        ; recipient : Address.Stable.V1.t option
        ; status : Status.Stable.V1.t
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving sexp, bin_io, compare, equal, stable_witness]
    end
  end

  include Stable.V1
end

module Send_error = struct
  type t =
    | Invalid_message of Error.t
    | Rejected of
        { code : int
        ; message : string
        }
    | Temporary of { message : string }
    | Transport of Error.t
  [@@deriving sexp_of]

  let is_retryable = function
    | Temporary _ | Transport _ -> true
    | Invalid_message _ | Rejected _ -> false
  ;;
end

module type S = sig
  type t

  val send
    :  t
    -> ?idempotency_key:string
    -> Message.t
    -> (Delivery_id.t, Send_error.t) Result.t Deferred.t

  val events : t -> Event.t Pipe.Reader.t
end

type t = T : (module S with type t = 'a) * 'a -> t

let pack m x = T (m, x)
let send (T ((module M), x)) ?idempotency_key message = M.send x ?idempotency_key message
let events (T ((module M), x)) = M.events x

module Event_feed = struct
  type t = { mutable subscribers : Event.t Pipe.Writer.t list }

  let create () = { subscribers = [] }

  let publish t event =
    t.subscribers <- List.filter t.subscribers ~f:(fun w -> not (Pipe.is_closed w));
    List.iter t.subscribers ~f:(fun w -> Pipe.write_without_pushback w event)
  ;;

  let subscribe t =
    let r, w = Pipe.create () in
    t.subscribers <- t.subscribers @ [ w ];
    r
  ;;
end

module Idempotency = struct
  type entry =
    { message : Message.t
    ; result : (Delivery_id.t, Send_error.t) Result.t Deferred.t
    }

  type t =
    { capacity : int
    ; entries : (string, entry) Hashtbl.t
    ; order : string Queue.t
    }

  let create ?(capacity = 10_000) () =
    { capacity; entries = Hashtbl.create (module String); order = Queue.create () }
  ;;

  let remember t key entry =
    Hashtbl.set t.entries ~key ~data:entry;
    Queue.enqueue t.order key;
    while Queue.length t.order > t.capacity do
      Hashtbl.remove t.entries (Queue.dequeue_exn t.order)
    done
  ;;

  let forget t key result =
    match Hashtbl.find t.entries key with
    | Some entry when phys_equal entry.result result -> Hashtbl.remove t.entries key
    | _ -> ()
  ;;

  let run t ~key message ~send =
    match key with
    | None -> send ()
    | Some key ->
      (match Hashtbl.find t.entries key with
       | Some entry when Message.equal entry.message message -> entry.result
       | Some _ ->
         return
           (Error
              (Send_error.Invalid_message
                 (Error.create_s
                    [%message "idempotency key reused with a different message" key])))
       | None ->
         let result = send () in
         remember t key { message; result };
         upon result (function
           | Ok _ -> ()
           | Error _ -> forget t key result);
         result)
  ;;
end
