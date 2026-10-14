open! Core

module Locale = struct
  module T = struct
    type t = string [@@deriving compare, equal, hash, sexp_of]
  end

  let of_string s =
    let error () = Or_error.error_s [%message "invalid locale" s] in
    let s' = String.tr s ~target:'_' ~replacement:'-' in
    let language_ok l =
      (String.length l = 2 || String.length l = 3) && String.for_all l ~f:Char.is_alpha
    in
    match String.split s' ~on:'-' with
    | [ l ] when language_ok l -> Ok (String.lowercase l)
    | [ l; r ] when language_ok l ->
      if String.length r = 2 && String.for_all r ~f:Char.is_alpha
      then Ok (String.lowercase l ^ "-" ^ String.uppercase r)
      else if String.length r = 3 && String.for_all r ~f:Char.is_digit
      then Ok (String.lowercase l ^ "-" ^ r)
      else error ()
    | _ -> error ()
  ;;

  let of_string_exn s = Or_error.ok_exn (of_string s)
  let to_string t = t
  let language t = List.hd_exn (String.split t ~on:'-')
  let en = "en"
  let de = "de"
  let fr = "fr"
  let es = "es"
  let it = "it"
  let pt = "pt"
  let ru = "ru"
  let ky = "ky"

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
end

module Rendered = struct
  type t =
    { subject : string
    ; text : string
    ; html : string option
    }
  [@@deriving sexp, compare, equal]
end

module Html = struct
  let escape s =
    String.concat_map s ~f:(function
      | '&' -> "&amp;"
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '"' -> "&quot;"
      | '\'' -> "&#39;"
      | c -> String.of_char c)
  ;;
end

type 'params t =
  { name : string
  ; samples : 'params list
  ; must_mention : 'params -> string list
  ; renderers : (Locale.t * ('params -> Rendered.t)) list
  ; review_pending : Locale.t list
  }

let create ~name ~samples ?(must_mention = fun _ -> []) ?(review_pending = []) renderers =
  if String.is_empty name then raise_s [%message "Template.create: empty name"];
  if List.is_empty renderers then raise_s [%message "Template.create: no renderers" name];
  if List.is_empty samples then raise_s [%message "Template.create: no samples" name];
  (match List.find_a_dup (List.map renderers ~f:fst) ~compare:Locale.compare with
   | Some locale ->
     raise_s [%message "Template.create: locale given twice" name (locale : Locale.t)]
   | None -> ());
  List.iter review_pending ~f:(fun locale ->
    if not (List.Assoc.mem renderers locale ~equal:Locale.equal)
    then
      raise_s
        [%message
          "Template.create: review pending for a locale without a renderer"
            name
            (locale : Locale.t)]);
  { name
  ; samples
  ; must_mention
  ; renderers
  ; review_pending = List.dedup_and_sort review_pending ~compare:Locale.compare
  }
;;

let review_pending t = t.review_pending
let name t = t.name
let locales t = List.map t.renderers ~f:fst
let has t locale = List.Assoc.mem t.renderers locale ~equal:Locale.equal

let resolve t locale =
  if has t locale
  then locale
  else if has t (Locale.language locale)
  then Locale.language locale
  else fst (List.hd_exn t.renderers)
;;

let render t ~locale params =
  let f = List.Assoc.find_exn t.renderers (resolve t locale) ~equal:Locale.equal in
  f params
;;

let to_message t ~locale params ~from ~to_ ?headers () =
  let { Rendered.subject; text; html } = render t ~locale params in
  Message.create ~from ~to_ ?headers ?html ~subject ~text ()
;;

module Catalogue = struct
  type template = T : _ t -> template

  module Problem = struct
    type t =
      { template : string
      ; locale : Locale.t option
      ; problem : string
      }
    [@@deriving sexp, compare, equal]
  end

  let void_elements =
    [ "area"
    ; "base"
    ; "br"
    ; "col"
    ; "embed"
    ; "hr"
    ; "img"
    ; "input"
    ; "link"
    ; "meta"
    ; "source"
    ; "track"
    ; "wbr"
    ]
  ;;

  (* Reads tag names in order and checks that non-void elements nest and close. Comments
     and doctypes are skipped. Good enough to catch a missing [</p>]; it is not an HTML
     parser. *)
  let html_balance html =
    let rec go i stack =
      match String.index_from html i '<' with
      | None ->
        (match stack with
         | [] -> None
         | open_ :: _ -> Some (sprintf "<%s> is never closed" open_))
      | Some start ->
        if String.is_substring_at html ~pos:start ~substring:"<!--"
        then (
          match String.substr_index html ~pos:start ~pattern:"-->" with
          | None -> Some "unterminated comment"
          | Some stop -> go (stop + 3) stack)
        else (
          match String.index_from html start '>' with
          | None -> Some "unterminated tag"
          | Some stop ->
            let inner = String.sub html ~pos:(start + 1) ~len:(stop - start - 1) in
            let closing = String.is_prefix inner ~prefix:"/" in
            let self_closing = String.is_suffix inner ~suffix:"/" in
            let name =
              String.lowercase
                (String.take_while
                   (String.lstrip ~drop:(Char.equal '/') inner)
                   ~f:(fun c -> Char.is_alphanum c || Char.equal c '-'))
            in
            if String.is_prefix inner ~prefix:"!" || String.is_empty name
            then go (stop + 1) stack
            else if closing
            then (
              match stack with
              | top :: rest when String.equal top name -> go (stop + 1) rest
              | top :: _ -> Some (sprintf "</%s> closes <%s>" name top)
              | [] -> Some (sprintf "</%s> without an opening tag" name))
            else if self_closing || List.mem void_elements name ~equal:String.equal
            then go (stop + 1) stack
            else go (stop + 1) (name :: stack))
    in
    go 0 []
  ;;

  (* Characters, not bytes: a Cyrillic or Greek subject takes two bytes a letter. UTF-8
     continuation bytes (10xxxxxx) do not start a character. *)
  let characters s = String.count s ~f:(fun c -> Char.to_int c land 0xC0 <> 0x80)

  let check_rendering ~mention (r : Rendered.t) =
    List.concat
      [ (if String.is_empty (String.strip r.subject) then [ "empty subject" ] else [])
      ; (if String.exists r.subject ~f:(fun c -> Char.equal c '\n' || Char.equal c '\r')
         then [ "line break in subject" ]
         else [])
      ; (if characters r.subject > 150 then [ "subject over 150 characters" ] else [])
      ; (if String.is_empty (String.strip r.text) then [ "empty text body" ] else [])
      ; List.filter_map mention ~f:(fun m ->
          Option.some_if
            (not (String.is_substring r.text ~substring:m))
            (sprintf "text does not mention %S" m))
      ; (match r.html with
         | None -> []
         | Some html ->
           List.concat
             [ List.filter_map mention ~f:(fun m ->
                 Option.some_if
                   (not (String.is_substring html ~substring:(Html.escape m)))
                   (sprintf "HTML does not mention %S" m))
             ; List.filter_map mention ~f:(fun m ->
                 Option.some_if
                   ((not (String.equal m (Html.escape m)))
                    && String.is_substring html ~substring:m)
                   (sprintf "HTML contains %S unescaped" m))
             ; Option.to_list (html_balance html)
             ])
      ]
  ;;

  let review_pending templates =
    List.concat_map templates ~f:(fun (T t) ->
      List.map t.review_pending ~f:(fun locale -> t.name, locale))
  ;;

  let check ?(required_locales = []) templates =
    let duplicates =
      List.map templates ~f:(fun (T t) -> t.name)
      |> List.sort_and_group ~compare:String.compare
      |> List.filter_map ~f:(function
        | name :: _ :: _ ->
          Some { Problem.template = name; locale = None; problem = "name used twice" }
        | _ -> None)
    in
    let per_template (T t) =
      let missing =
        List.filter_map required_locales ~f:(fun locale ->
          Option.some_if
            (not (has t locale))
            { Problem.template = t.name
            ; locale = Some locale
            ; problem = "missing locale"
            })
      in
      let renderings =
        List.concat_map t.renderers ~f:(fun (locale, f) ->
          List.concat_map t.samples ~f:(fun sample ->
            let problems =
              match Or_error.try_with (fun () -> f sample) with
              | Error e -> [ "renderer raised: " ^ Error.to_string_hum e ]
              | Ok r -> check_rendering ~mention:(t.must_mention sample) r
            in
            List.map problems ~f:(fun problem ->
              { Problem.template = t.name; locale = Some locale; problem })))
      in
      missing @ renderings
    in
    duplicates @ List.concat_map templates ~f:per_template
    |> List.dedup_and_sort ~compare:Problem.compare
  ;;
end
