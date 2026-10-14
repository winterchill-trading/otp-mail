open! Core
open Otp_mail

val address : string -> Address.t
val mailbox : ?name:string -> string -> Mailbox.t
val show_wire : string -> unit
val fixed_date : Time_ns.t
val fixed_message_id : Message_id.t
