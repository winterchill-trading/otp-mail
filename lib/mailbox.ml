open! Core

module Stable = struct
  module V1 = struct
    type t =
      { display_name : string option
      ; address : Address.Stable.V1.t
      }
    [@@deriving sexp, bin_io, compare, equal, hash, stable_witness]
  end
end

include Stable.V1

let clean_name name =
  let name =
    String.map name ~f:(fun c ->
      if Char.to_int c < 32 || Char.to_int c = 127 then ' ' else c)
    |> String.strip
  in
  Option.some_if (not (String.is_empty name)) name
;;

let create ?display_name address =
  { display_name = Option.bind display_name ~f:clean_name; address }
;;

let of_address address = { display_name = None; address }
let is_atext c = Char.is_alphanum c || String.mem "!#$%&'*+-/=?^_`{|}~" c

let phrase name =
  if Mime.Encoded_word.needs_encoding name
  then Mime.Encoded_word.encode name
  else if List.for_all (String.split name ~on:' ') ~f:(fun w ->
            (not (String.is_empty w)) && String.for_all w ~f:is_atext)
  then name
  else
    "\""
    ^ String.concat_map name ~f:(fun c ->
      match c with
      | '"' | '\\' -> "\\" ^ String.of_char c
      | c -> String.of_char c)
    ^ "\""
;;

let to_string t =
  match t.display_name with
  | None -> Address.to_string t.address
  | Some name -> phrase name ^ " <" ^ Address.to_string t.address ^ ">"
;;

let unquote s =
  let n = String.length s in
  if n >= 2 && Char.equal s.[0] '"' && Char.equal s.[n - 1] '"'
  then (
    let buf = Buffer.create n in
    let rec go i =
      if i < n - 1
      then
        if Char.equal s.[i] '\\' && i + 1 < n - 1
        then (
          Buffer.add_char buf s.[i + 1];
          go (i + 2))
        else (
          Buffer.add_char buf s.[i];
          go (i + 1))
    in
    go 1;
    Buffer.contents buf)
  else Mime.Encoded_word.decode s
;;

let of_string s =
  let open Or_error.Let_syntax in
  let s = String.strip s in
  if String.is_suffix s ~suffix:">"
  then (
    match String.rsplit2 s ~on:'<' with
    | None -> Or_error.error_s [%message "mailbox: unbalanced angle brackets" s]
    | Some (name, rest) ->
      let%map address = Address.of_string (String.chop_suffix_exn rest ~suffix:">") in
      create ~display_name:(unquote (String.strip name)) address)
  else (
    let%map address = Address.of_string s in
    of_address address)
;;

let list_to_string ts = String.concat ~sep:", " (List.map ts ~f:to_string)

let split_list s =
  let parts = ref [] in
  let buf = Buffer.create (String.length s) in
  let in_quotes = ref false
  and in_angle = ref false
  and escaped = ref false in
  String.iter s ~f:(fun c ->
    if !escaped
    then (
      escaped := false;
      Buffer.add_char buf c)
    else (
      match c with
      | '\\' when !in_quotes ->
        escaped := true;
        Buffer.add_char buf c
      | '"' ->
        in_quotes := not !in_quotes;
        Buffer.add_char buf c
      | '<' when not !in_quotes ->
        in_angle := true;
        Buffer.add_char buf c
      | '>' when not !in_quotes ->
        in_angle := false;
        Buffer.add_char buf c
      | ',' when not (!in_quotes || !in_angle) ->
        parts := Buffer.contents buf :: !parts;
        Buffer.clear buf
      | c -> Buffer.add_char buf c));
  parts := Buffer.contents buf :: !parts;
  List.rev !parts
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun p -> not (String.is_empty p))
;;

let list_of_string s = List.map (split_list s) ~f:of_string |> Or_error.all
