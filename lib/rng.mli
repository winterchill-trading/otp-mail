(** Sources of random bytes.

    {!system} is the one to use for codes and identifiers. {!of_seed} is deterministic and
    exists for tests, examples and simulations that must print the same output on every
    run. *)

open! Core

type t

(** [system ()] reads the kernel CSPRNG through [/dev/urandom] (Linux, macOS, BSDs). *)
val system : unit -> t

(** [of_seed seed] is a ChaCha20-based generator seeded with [seed] (at least 16 bytes).
    Never use it for real codes. *)
val of_seed : string -> t

(** [bytes t n] is [n] random bytes. *)
val bytes : t -> int -> string

(** [digits t n] is [n] decimal digits, each uniform: bytes of 250 and above are discarded
    so that every digit is equally likely. *)
val digits : t -> int -> string

(** [hex t n] is [n] random bytes in lower-case hex. *)
val hex : t -> int -> string
