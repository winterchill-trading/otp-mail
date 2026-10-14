open! Core

let is_atext c = Char.is_alphanum c || String.mem "!#$%&'*+-/=?^_`{|}~" c

let is_dot_atom s =
  (not (String.is_empty s))
  && List.for_all (String.split s ~on:'.') ~f:(fun atom ->
    (not (String.is_empty atom)) && String.for_all atom ~f:is_atext)
;;

let create ~local ~domain =
  if is_dot_atom local && is_dot_atom domain
  then Ok (local ^ "@" ^ domain)
  else Or_error.error_s [%message "invalid message id" local domain]
;;

let of_string s =
  let s = String.strip s in
  let s =
    if String.is_prefix s ~prefix:"<" && String.is_suffix s ~suffix:">"
    then String.sub s ~pos:1 ~len:(String.length s - 2)
    else s
  in
  match String.rsplit2 s ~on:'@' with
  | None -> Or_error.error_s [%message "invalid message id" s]
  | Some (local, domain) -> create ~local ~domain
;;

let generate ~rng ~domain = Or_error.ok_exn (create ~local:(Rng.hex rng 16) ~domain)
let to_string t = "<" ^ t ^ ">"
let local t = fst (String.rsplit2_exn t ~on:'@')

module Stable = struct
  module V1 = struct
    type t = string [@@deriving compare, equal, hash, sexp_of, stable_witness]

    let t_of_sexp sexp =
      match of_string (String.t_of_sexp sexp) with
      | Ok t -> t
      | Error e -> of_sexp_error (Error.to_string_hum e) sexp
    ;;

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

include Stable.V1
