open! Core

let hex_digit n = "0123456789ABCDEF".[n]

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
  | 'A' .. 'F' -> Some (Char.to_int c - Char.to_int 'A' + 10)
  | 'a' .. 'f' -> Some (Char.to_int c - Char.to_int 'a' + 10)
  | _ -> None
;;

(* Lines split on LF with a trailing CR removed. *)
let lines_of s =
  String.split s ~on:'\n'
  |> List.map ~f:(fun l -> String.chop_suffix_if_exists l ~suffix:"\r")
;;

module Base64 = struct
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let encode_one_line s =
    let n = String.length s in
    let out = Buffer.create (((n + 2) / 3 * 4) + 1) in
    let byte i = Char.to_int s.[i] in
    let rec go i =
      if i + 3 <= n
      then (
        let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
        Buffer.add_char out alphabet.[(v lsr 18) land 63];
        Buffer.add_char out alphabet.[(v lsr 12) land 63];
        Buffer.add_char out alphabet.[(v lsr 6) land 63];
        Buffer.add_char out alphabet.[v land 63];
        go (i + 3))
      else if i + 2 = n
      then (
        let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) in
        Buffer.add_char out alphabet.[(v lsr 18) land 63];
        Buffer.add_char out alphabet.[(v lsr 12) land 63];
        Buffer.add_char out alphabet.[(v lsr 6) land 63];
        Buffer.add_char out '=')
      else if i + 1 = n
      then (
        let v = byte i lsl 16 in
        Buffer.add_char out alphabet.[(v lsr 18) land 63];
        Buffer.add_char out alphabet.[(v lsr 12) land 63];
        Buffer.add_string out "==")
    in
    go 0;
    Buffer.contents out
  ;;

  let encode ?(line_length = 76) s =
    let line_length = line_length / 4 * 4 in
    let flat = encode_one_line s in
    if line_length <= 0 || String.length flat <= line_length
    then flat
    else (
      let n = String.length flat in
      let rec chunks i acc =
        if i >= n
        then List.rev acc
        else
          chunks
            (i + line_length)
            (String.sub flat ~pos:i ~len:(min line_length (n - i)) :: acc)
      in
      String.concat ~sep:"\r\n" (chunks 0 []))
  ;;

  let value c =
    match c with
    | 'A' .. 'Z' -> Some (Char.to_int c - Char.to_int 'A')
    | 'a' .. 'z' -> Some (Char.to_int c - Char.to_int 'a' + 26)
    | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0' + 52)
    | '+' -> Some 62
    | '/' -> Some 63
    | _ -> None
  ;;

  let decode s =
    let chars = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) in
    let n = String.length chars in
    if n % 4 <> 0
    then Or_error.error_s [%message "base64: length is not a multiple of 4" (n : int)]
    else (
      let out = Buffer.create (n / 4 * 3) in
      let rec go i =
        if i >= n
        then Ok (Buffer.contents out)
        else (
          let last = i + 4 = n in
          let c j = chars.[i + j] in
          match value (c 0), value (c 1), c 2, c 3 with
          | Some a, Some b, '=', '=' when last ->
            Buffer.add_char out (Char.of_int_exn ((a lsl 2) lor (b lsr 4) land 255));
            go (i + 4)
          | Some a, Some b, c2, '=' when last ->
            (match value c2 with
             | None -> Or_error.error_s [%message "base64: bad character" (c2 : char)]
             | Some c ->
               let v = (a lsl 18) lor (b lsl 12) lor (c lsl 6) in
               Buffer.add_char out (Char.of_int_exn ((v lsr 16) land 255));
               Buffer.add_char out (Char.of_int_exn ((v lsr 8) land 255));
               go (i + 4))
          | Some a, Some b, c2, c3 ->
            (match value c2, value c3 with
             | Some c, Some d ->
               let v = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
               Buffer.add_char out (Char.of_int_exn ((v lsr 16) land 255));
               Buffer.add_char out (Char.of_int_exn ((v lsr 8) land 255));
               Buffer.add_char out (Char.of_int_exn (v land 255));
               go (i + 4)
             | _ ->
               Or_error.error_s
                 [%message "base64: bad character or padding" ~at:(i : int)])
          | _ -> Or_error.error_s [%message "base64: bad character" ~at:(i : int)])
      in
      go 0)
  ;;
end

module Quoted_printable = struct
  let max_line = 76

  let is_literal c =
    let code = Char.to_int c in
    code >= 33 && code <= 126 && not (Char.equal c '=')
  ;;

  (* Tokens of one logical line: each token is what one input byte becomes. *)
  let tokens_of_line line ~binary ~ends_input =
    let n = String.length line in
    List.init n ~f:(fun i ->
      let c = line.[i] in
      let at_end = i = n - 1 in
      let whitespace = Char.equal c ' ' || Char.equal c '\t' in
      if is_literal c
      then String.of_char c
      else if whitespace && not (at_end && ((not binary) || ends_input))
      then String.of_char c
      else (
        let code = Char.to_int c in
        String.of_char_list [ '='; hex_digit (code lsr 4); hex_digit (code land 15) ]))
  ;;

  (* Wraps tokens with soft line breaks so that no line exceeds [max_line]. *)
  let add_wrapped buf tokens =
    let rec go len = function
      | [] -> ()
      | [ tok ] when len + String.length tok <= max_line -> Buffer.add_string buf tok
      | tok :: rest ->
        let tlen = String.length tok in
        if len + tlen > max_line - 1
        then (
          Buffer.add_string buf "=\r\n";
          Buffer.add_string buf tok;
          go tlen rest)
        else (
          Buffer.add_string buf tok;
          go (len + tlen) rest)
    in
    go 0 tokens
  ;;

  (* Splits text into logical lines on CRLF or LF; a lone CR stays in the line. *)
  let text_lines s =
    let lines = String.split s ~on:'\n' in
    let last = List.length lines - 1 in
    List.mapi lines ~f:(fun i l ->
      if i < last then String.chop_suffix_if_exists l ~suffix:"\r" else l)
  ;;

  let encode ?(binary = false) s =
    let buf = Buffer.create (String.length s * 3 / 2) in
    if binary
    then add_wrapped buf (tokens_of_line s ~binary ~ends_input:true)
    else (
      let lines = text_lines s in
      List.iteri lines ~f:(fun i line ->
        if i > 0 then Buffer.add_string buf "\r\n";
        add_wrapped buf (tokens_of_line line ~binary ~ends_input:false)));
    Buffer.contents buf
  ;;

  let decode s =
    let buf = Buffer.create (String.length s) in
    let lines = lines_of s in
    let count = List.length lines in
    let rec decode_line line i ~len =
      if i >= len
      then Ok ()
      else (
        match line.[i] with
        | '=' when i + 2 < len ->
          (match hex_value line.[i + 1], hex_value line.[i + 2] with
           | Some a, Some b ->
             Buffer.add_char buf (Char.of_int_exn ((a lsl 4) lor b));
             decode_line line (i + 3) ~len
           | _ ->
             Or_error.error_s [%message "quoted-printable: bad escape" (line : string)])
        | '=' ->
          Or_error.error_s [%message "quoted-printable: bad escape" (line : string)]
        | c ->
          Buffer.add_char buf c;
          decode_line line (i + 1) ~len)
    in
    List.foldi lines ~init:(Ok ()) ~f:(fun idx acc line ->
      Result.bind acc ~f:(fun () ->
        let line =
          String.rstrip line ~drop:(fun c -> Char.equal c ' ' || Char.equal c '\t')
        in
        let len = String.length line in
        let soft = len > 0 && Char.equal line.[len - 1] '=' in
        let len = if soft then len - 1 else len in
        Result.map (decode_line line 0 ~len) ~f:(fun () ->
          if (not soft) && idx < count - 1 then Buffer.add_string buf "\r\n")))
    |> Result.map ~f:(fun () -> Buffer.contents buf)
  ;;
end

module Encoded_word = struct
  let needs_encoding s =
    String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      (code < 32 && not (Char.equal c '\t')) || code > 126)
    || String.is_substring s ~substring:"=?"
  ;;

  (* Length of the UTF-8 sequence that starts with [c]; stray bytes count as one. *)
  let utf8_length c =
    let code = Char.to_int c in
    if code < 0xC0 then 1 else if code < 0xE0 then 2 else if code < 0xF0 then 3 else 4
  ;;

  (* 45 bytes of text become 60 base64 characters, and with the 12 characters of
     "=?UTF-8?B?" and "?=" a word stays within the 75 that RFC 2047 allows. *)
  let max_chunk = 45

  let chunks s =
    let n = String.length s in
    let rec go start i acc =
      if i >= n
      then
        List.rev
          (if i > start then String.sub s ~pos:start ~len:(i - start) :: acc else acc)
      else (
        let l = min (utf8_length s.[i]) (n - i) in
        if i + l - start > max_chunk
        then go i (i + l) (String.sub s ~pos:start ~len:(i - start) :: acc)
        else go start (i + l) acc)
    in
    go 0 0 []
  ;;

  let encode s =
    if not (needs_encoding s)
    then s
    else
      chunks s
      |> List.map ~f:(fun chunk ->
        "=?UTF-8?B?" ^ Base64.encode ~line_length:0 chunk ^ "?=")
      |> String.concat ~sep:" "
  ;;

  let latin1_to_utf8 s =
    let buf = Buffer.create (String.length s * 2) in
    String.iter s ~f:(fun c ->
      let code = Char.to_int c in
      if code < 0x80
      then Buffer.add_char buf c
      else (
        Buffer.add_char buf (Char.of_int_exn (0xC0 lor (code lsr 6)));
        Buffer.add_char buf (Char.of_int_exn (0x80 lor (code land 0x3F)))));
    Buffer.contents buf
  ;;

  let decode_q text =
    let n = String.length text in
    let buf = Buffer.create n in
    let rec go i =
      if i >= n
      then Some (Buffer.contents buf)
      else (
        match text.[i] with
        | '_' ->
          Buffer.add_char buf ' ';
          go (i + 1)
        | '=' when i + 2 < n ->
          (match hex_value text.[i + 1], hex_value text.[i + 2] with
           | Some a, Some b ->
             Buffer.add_char buf (Char.of_int_exn ((a lsl 4) lor b));
             go (i + 3)
           | _ -> None)
        | '=' -> None
        | c ->
          Buffer.add_char buf c;
          go (i + 1))
    in
    go 0
  ;;

  (* Tries to read an encoded word starting at [i]; returns its text and the index just
     past it. *)
  let read_word s i =
    let n = String.length s in
    let field_end start =
      let rec go j =
        if j >= n
        then None
        else if Char.equal s.[j] '?'
        then Some j
        else if Char.is_whitespace s.[j]
        then None
        else go (j + 1)
      in
      go start
    in
    if i + 1 >= n || not (Char.equal s.[i] '=' && Char.equal s.[i + 1] '?')
    then None
    else
      let open Option.Let_syntax in
      let%bind q1 = field_end (i + 2) in
      let charset = String.sub s ~pos:(i + 2) ~len:(q1 - i - 2) in
      let%bind q2 = field_end (q1 + 1) in
      let encoding = String.sub s ~pos:(q1 + 1) ~len:(q2 - q1 - 1) in
      let%bind q3 = field_end (q2 + 1) in
      let%bind () = Option.some_if (q3 + 1 < n && Char.equal s.[q3 + 1] '=') () in
      let text = String.sub s ~pos:(q2 + 1) ~len:(q3 - q2 - 1) in
      let%bind bytes =
        match String.lowercase encoding with
        | "b" -> Base64.decode text |> Result.ok
        | "q" -> decode_q text
        | _ -> None
      in
      let charset = String.lowercase (List.hd_exn (String.split charset ~on:'*')) in
      let%map decoded =
        match charset with
        | "utf-8" | "us-ascii" -> Some bytes
        | "iso-8859-1" | "latin1" -> Some (latin1_to_utf8 bytes)
        | _ -> None
      in
      decoded, q3 + 2
  ;;

  type segment =
    | Text of string
    | Word of string

  let decode s =
    let n = String.length s in
    let segments = ref [] in
    let text = Buffer.create 16 in
    let flush () =
      if Buffer.length text > 0
      then (
        segments := Text (Buffer.contents text) :: !segments;
        Buffer.clear text)
    in
    let rec go i =
      if i < n
      then (
        match read_word s i with
        | Some (decoded, next) ->
          flush ();
          segments := Word decoded :: !segments;
          go next
        | None ->
          Buffer.add_char text s.[i];
          go (i + 1))
    in
    go 0;
    flush ();
    let segments = Array.of_list (List.rev !segments) in
    let len = Array.length segments in
    let buf = Buffer.create n in
    Array.iteri segments ~f:(fun i seg ->
      match seg with
      | Word w -> Buffer.add_string buf w
      | Text t ->
        let between_words =
          i > 0
          && i < len - 1
          && (match segments.(i - 1), segments.(i + 1) with
              | Word _, Word _ -> true
              | _ -> false)
          && String.for_all t ~f:Char.is_whitespace
        in
        if not between_words then Buffer.add_string buf t);
    Buffer.contents buf
  ;;
end

module Header = struct
  type t = (string * string) list [@@deriving sexp_of]

  let valid_name s =
    (not (String.is_empty s))
    && String.for_all s ~f:(fun c ->
      let code = Char.to_int c in
      code >= 33 && code <= 126 && not (Char.equal c ':'))
  ;;

  let max_line = 78

  let fold ~name value =
    let value =
      String.map value ~f:(fun c ->
        if Char.equal c '\r' || Char.equal c '\n' then ' ' else c)
    in
    let tokens = String.split (name ^ ": " ^ value) ~on:' ' in
    let buf = Buffer.create (String.length value + String.length name + 8) in
    (match tokens with
     | [] -> ()
     | first :: rest ->
       Buffer.add_string buf first;
       let (_ : int) =
         List.fold rest ~init:(String.length first) ~f:(fun len tok ->
           let tlen = String.length tok in
           if len + 1 + tlen > max_line && tlen > 0
           then (
             Buffer.add_string buf "\r\n ";
             Buffer.add_string buf tok;
             1 + tlen)
           else (
             Buffer.add_char buf ' ';
             Buffer.add_string buf tok;
             len + 1 + tlen))
       in
       ());
    Buffer.add_string buf "\r\n";
    Buffer.contents buf
  ;;

  let unfold s =
    let n = String.length s in
    let buf = Buffer.create n in
    let rec go i =
      if i < n
      then (
        let is_wsp j = j < n && (Char.equal s.[j] ' ' || Char.equal s.[j] '\t') in
        if Char.equal s.[i] '\r'
           && i + 1 < n
           && Char.equal s.[i + 1] '\n'
           && is_wsp (i + 2)
        then go (i + 2)
        else if Char.equal s.[i] '\n' && is_wsp (i + 1)
        then go (i + 1)
        else (
          Buffer.add_char buf s.[i];
          go (i + 1)))
    in
    go 0;
    Buffer.contents buf
  ;;

  let find t name =
    List.find_map t ~f:(fun (n, v) -> Option.some_if (String.Caseless.equal n name) v)
  ;;

  let find_all t name =
    List.filter_map t ~f:(fun (n, v) -> Option.some_if (String.Caseless.equal n name) v)
  ;;

  let parse s =
    let lines = lines_of s in
    let rec take acc = function
      | [] -> List.rev acc
      | "" :: _ -> List.rev acc
      | l :: rest -> take (l :: acc) rest
    in
    let lines = take [] lines in
    let is_continuation l =
      String.length l > 0 && (Char.equal l.[0] ' ' || Char.equal l.[0] '\t')
    in
    let rec go acc current = function
      | [] -> Ok (List.rev (Option.to_list current @ acc))
      | l :: rest when is_continuation l ->
        (match current with
         | None -> Or_error.error_s [%message "header: continuation before any field" l]
         | Some (n, v) -> go acc (Some (n, v ^ l)) rest)
      | l :: rest ->
        (match String.lsplit2 l ~on:':' with
         | None -> Or_error.error_s [%message "header: line without a colon" l]
         | Some (n, v) ->
           let n = String.rstrip n in
           if not (valid_name n)
           then Or_error.error_s [%message "header: bad field name" n]
           else go (Option.to_list current @ acc) (Some (n, v)) rest)
    in
    go [] None lines |> Or_error.map ~f:(List.map ~f:(fun (n, v) -> n, String.strip v))
  ;;

  let to_string t = String.concat (List.map t ~f:(fun (name, value) -> fold ~name value))
end

module Content_type = struct
  type t =
    { type_ : string
    ; subtype : string
    ; params : (string * string) list
    }
  [@@deriving sexp, compare, equal]

  let create ?(params = []) type_ subtype =
    { type_ = String.lowercase type_
    ; subtype = String.lowercase subtype
    ; params = List.map params ~f:(fun (n, v) -> String.lowercase n, v)
    }
  ;;

  let default = create "text" "plain" ~params:[ "charset", "us-ascii" ]
  let mime_type t = t.type_ ^ "/" ^ t.subtype

  let param t name =
    List.find_map t.params ~f:(fun (n, v) ->
      Option.some_if (String.Caseless.equal n name) v)
  ;;

  let is_tspecial c = String.mem "()<>@,;:\\\"/[]?=" c

  let is_token_char c =
    let code = Char.to_int c in
    code > 32 && code < 127 && not (is_tspecial c)
  ;;

  let parse s =
    let n = String.length s in
    let skip_ws i =
      let rec go i = if i < n && Char.is_whitespace s.[i] then go (i + 1) else i in
      go i
    in
    let token i =
      let rec go j = if j < n && is_token_char s.[j] then go (j + 1) else j in
      let j = go i in
      if j = i then None else Some (String.sub s ~pos:i ~len:(j - i), j)
    in
    let quoted i =
      (* [i] is just past the opening quote. *)
      let buf = Buffer.create 16 in
      let rec go j =
        if j >= n
        then None
        else (
          match s.[j] with
          | '"' -> Some (Buffer.contents buf, j + 1)
          | '\\' when j + 1 < n ->
            Buffer.add_char buf s.[j + 1];
            go (j + 2)
          | c ->
            Buffer.add_char buf c;
            go (j + 1))
      in
      go i
    in
    let error () = Or_error.error_s [%message "content-type: cannot parse" s] in
    let rec params i acc =
      let i = skip_ws i in
      if i >= n
      then Some (List.rev acc)
      else if not (Char.equal s.[i] ';')
      then None
      else (
        let i = skip_ws (i + 1) in
        if i >= n
        then Some (List.rev acc)
        else
          Option.bind (token i) ~f:(fun (name, i) ->
            let i = skip_ws i in
            if i >= n || not (Char.equal s.[i] '=')
            then None
            else (
              let i = skip_ws (i + 1) in
              let value =
                if i < n && Char.equal s.[i] '"' then quoted (i + 1) else token i
              in
              Option.bind value ~f:(fun (value, i) ->
                params i ((String.lowercase name, value) :: acc)))))
    in
    let i = skip_ws 0 in
    match token i with
    | None -> error ()
    | Some (type_, i) ->
      if i >= n || not (Char.equal s.[i] '/')
      then error ()
      else (
        match token (i + 1) with
        | None -> error ()
        | Some (subtype, i) ->
          (match params i [] with
           | None -> error ()
           | Some params ->
             Ok
               { type_ = String.lowercase type_
               ; subtype = String.lowercase subtype
               ; params
               }))
  ;;

  let to_string t =
    let value v =
      if (not (String.is_empty v)) && String.for_all v ~f:is_token_char
      then v
      else (
        let escaped =
          String.concat_map v ~f:(fun c ->
            match c with
            | '"' | '\\' -> "\\" ^ String.of_char c
            | c -> String.of_char c)
        in
        "\"" ^ escaped ^ "\"")
    in
    String.concat
      (mime_type t :: List.map t.params ~f:(fun (n, v) -> "; " ^ n ^ "=" ^ value v))
  ;;
end

module Entity = struct
  type body =
    | Leaf of string
    | Multipart of t list

  and t =
    { headers : Header.t
    ; content_type : Content_type.t
    ; body : body
    }
  [@@deriving sexp_of]

  (* Splits at the first empty line, accepting CRLF or LF line breaks. *)
  let split_head_body s =
    let starts_blank =
      String.is_prefix s ~prefix:"\r\n" || String.is_prefix s ~prefix:"\n"
    in
    if starts_blank
    then
      ( ""
      , String.chop_prefix_if_exists
          (String.chop_prefix_if_exists s ~prefix:"\r")
          ~prefix:"\n" )
    else (
      let crlf = String.substr_index s ~pattern:"\r\n\r\n" in
      let lf = String.substr_index s ~pattern:"\n\n" in
      match crlf, lf with
      | Some a, Some b when b < a + 1 -> String.prefix s b, String.drop_prefix s (b + 2)
      | Some a, _ -> String.prefix s a, String.drop_prefix s (a + 4)
      | None, Some b -> String.prefix s b, String.drop_prefix s (b + 2)
      | None, None -> s, "")
  ;;

  (* The parts of a multipart body: the raw text between delimiter lines. Raw lines keep
     their CR, so joining them with LF restores the original bytes. *)
  let split_parts body ~boundary =
    let delimiter = "--" ^ boundary in
    let close = delimiter ^ "--" in
    let raw_lines = String.split body ~on:'\n' in
    let line_is target raw =
      let l = String.rstrip (String.chop_suffix_if_exists raw ~suffix:"\r") in
      String.equal l target
    in
    let finish part =
      let lines = List.rev part in
      (* The line break before a delimiter belongs to the delimiter. *)
      let text = String.concat ~sep:"\n" lines in
      String.chop_suffix_if_exists text ~suffix:"\r"
    in
    let rec go lines ~in_part ~current ~acc =
      match lines with
      | [] ->
        Or_error.error_s [%message "multipart: no closing boundary" (boundary : string)]
      | l :: _ when line_is close l ->
        if in_part then Ok (List.rev (finish current :: acc)) else Ok (List.rev acc)
      | l :: rest when line_is delimiter l ->
        let acc = if in_part then finish current :: acc else acc in
        go rest ~in_part:true ~current:[] ~acc
      | l :: rest ->
        if in_part
        then go rest ~in_part ~current:(l :: current) ~acc
        else go rest ~in_part ~current ~acc
    in
    go raw_lines ~in_part:false ~current:[] ~acc:[]
  ;;

  let rec parse s =
    let open Or_error.Let_syntax in
    let head, body = split_head_body s in
    let%bind headers = Header.parse head in
    let%bind content_type =
      match Header.find headers "Content-Type" with
      | None -> Ok Content_type.default
      | Some v -> Content_type.parse v
    in
    if String.equal content_type.type_ "multipart"
    then (
      match Content_type.param content_type "boundary" with
      | None -> Or_error.error_s [%message "multipart: no boundary parameter"]
      | Some boundary ->
        let%bind raw_parts = split_parts body ~boundary in
        let%map parts = List.map raw_parts ~f:parse |> Or_error.all in
        { headers; content_type; body = Multipart parts })
    else (
      let encoding =
        Header.find headers "Content-Transfer-Encoding"
        |> Option.value ~default:"7bit"
        |> String.strip
        |> String.lowercase
      in
      let%map content =
        match encoding with
        | "quoted-printable" -> Quoted_printable.decode body
        | "base64" -> Base64.decode body
        | "7bit" | "8bit" | "binary" -> Ok body
        | other -> Or_error.error_s [%message "unknown transfer encoding" other]
      in
      { headers; content_type; body = Leaf content })
  ;;

  let rec find t ~mime_type =
    match t.body with
    | Leaf _ ->
      if String.Caseless.equal (Content_type.mime_type t.content_type) mime_type
      then Some t
      else None
    | Multipart parts -> List.find_map parts ~f:(find ~mime_type)
  ;;

  let leaf_content t =
    match t.body with
    | Leaf s -> Some s
    | Multipart _ -> None
  ;;
end
