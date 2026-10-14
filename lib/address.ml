open! Core

module T = struct
  type t = string [@@deriving compare, equal, hash, sexp_of]
end

let is_atext c = Char.is_alphanum c || String.mem "!#$%&'*+-/=?^_`{|}~" c

(* A dot-atom: atoms of atext separated by single dots, none empty. *)
let is_dot_atom s =
  (not (String.is_empty s))
  && List.for_all (String.split s ~on:'.') ~f:(fun atom ->
    (not (String.is_empty atom)) && String.for_all atom ~f:is_atext)
;;

(* qtext is printable ASCII except backslash and double quote; space and tab are allowed
   because RFC 5322 lets folding whitespace appear inside the quotes. *)
let is_qtext c =
  let code = Char.to_int c in
  (code >= 32 && code <= 126 && not (Char.equal c '"' || Char.equal c '\\'))
  || Char.equal c '\t'
;;

let is_quoted_string s =
  let n = String.length s in
  n >= 2
  && Char.equal s.[0] '"'
  && Char.equal s.[n - 1] '"'
  &&
  let rec go i =
    if i >= n - 1
    then true
    else if Char.equal s.[i] '\\'
    then
      i + 1 < n - 1
      &&
      let code = Char.to_int s.[i + 1] in
      ((code >= 32 && code <= 126) || Char.equal s.[i + 1] '\t') && go (i + 2)
    else is_qtext s.[i] && go (i + 1)
  in
  go 1
;;

let is_domain_literal s =
  let n = String.length s in
  n >= 2
  && Char.equal s.[0] '['
  && Char.equal s.[n - 1] ']'
  && String.for_all
       (String.sub s ~pos:1 ~len:(n - 2))
       ~f:(fun c ->
         let code = Char.to_int c in
         code >= 33 && code <= 126 && not (String.mem "[]\\" c))
;;

let of_string s =
  let error reason = Or_error.error_s [%message "invalid email address" reason s] in
  if String.length s > 254
  then error "longer than 254 octets"
  else (
    match String.rsplit2 s ~on:'@' with
    | None -> error "no @"
    | Some (local, domain) ->
      if String.is_empty local
      then error "empty local part"
      else if String.length local > 64
      then error "local part longer than 64 octets"
      else if not (is_dot_atom local || is_quoted_string local)
      then error "bad local part"
      else if String.is_empty domain
      then error "empty domain"
      else if not (is_dot_atom domain || is_domain_literal domain)
      then error "bad domain"
      else Ok (local ^ "@" ^ String.lowercase domain))
;;

let of_string_exn s = Or_error.ok_exn (of_string s)
let to_string t = t
let local_part t = fst (String.rsplit2_exn t ~on:'@')
let domain t = snd (String.rsplit2_exn t ~on:'@')

let redacted t =
  let local = local_part t in
  String.prefix local 1 ^ "***@" ^ domain t
;;

let t_of_sexp sexp =
  match of_string (String.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> of_sexp_error (Error.to_string_hum e) sexp
;;

module Stable = struct
  module V1 = struct
    type t = string [@@deriving compare, equal, hash, sexp_of, stable_witness]

    let t_of_sexp = t_of_sexp

    (* The wire form is the string; reading validates it like [of_string]. *)
    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (String)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable s = of_string_exn s
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
