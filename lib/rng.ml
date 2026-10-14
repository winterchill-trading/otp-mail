open! Core

type t = Cryptokit.Random.rng

(* Cryptokit's [system_rng] is only implemented on Windows (it always raises on Unix in
   1.16), so read the kernel CSPRNG through /dev/urandom. *)
let system () = Cryptokit.Random.device_rng "/dev/urandom"

let of_seed seed =
  if String.length seed < 16
  then raise_s [%message "Rng.of_seed: the seed needs at least 16 bytes"];
  Cryptokit.Random.pseudo_rng seed
;;

let bytes (t : t) n = Cryptokit.Random.string t n

let digits t n =
  let buf = Buffer.create n in
  while Buffer.length buf < n do
    String.iter
      (bytes t (n - Buffer.length buf + 4))
      ~f:(fun c ->
        let b = Char.to_int c in
        (* 250 is the largest multiple of 10 that fits in a byte; keeping only bytes below
           it makes each digit exactly equally likely. *)
        if b < 250 && Buffer.length buf < n
        then Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + (b % 10))))
  done;
  Buffer.contents buf
;;

let hex t n = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) (bytes t n)
