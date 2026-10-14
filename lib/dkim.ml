open! Core

(* A minimal DER reader and writer: enough for RSA keys in PKCS#1, PKCS#8 and
   SubjectPublicKeyInfo form. *)
module Der = struct
  type tlv =
    { tag : int
    ; content : string
    }

  let read s ~pos =
    let n = String.length s in
    let byte i = Char.to_int s.[i] in
    if pos + 2 > n
    then Or_error.error_s [%message "DER: truncated"]
    else (
      let tag = byte pos in
      let first = byte (pos + 1) in
      let len_result =
        if first < 0x80
        then Ok (first, pos + 2)
        else (
          let count = first land 0x7f in
          if count = 0 || count > 4 || pos + 2 + count > n
          then Or_error.error_s [%message "DER: bad length"]
          else (
            let len = ref 0 in
            for i = 0 to count - 1 do
              len := (!len lsl 8) lor byte (pos + 2 + i)
            done;
            Ok (!len, pos + 2 + count)))
      in
      Or_error.bind len_result ~f:(fun (len, start) ->
        if start + len > n
        then Or_error.error_s [%message "DER: content runs past the end"]
        else Ok ({ tag; content = String.sub s ~pos:start ~len }, start + len)))
  ;;

  let read_all s =
    let rec go pos acc =
      if pos >= String.length s
      then Ok (List.rev acc)
      else Or_error.bind (read s ~pos) ~f:(fun (tlv, next) -> go next (tlv :: acc))
    in
    go 0 []
  ;;

  let expect_one s ~tag =
    match read s ~pos:0 with
    | Ok (tlv, _) when tlv.tag = tag -> Ok tlv.content
    | Ok (tlv, _) -> Or_error.error_s [%message "DER: unexpected tag" (tlv.tag : int)]
    | Error e -> Error e
  ;;

  let sequence s = Or_error.bind (expect_one s ~tag:0x30) ~f:read_all

  (* Integers are big-endian two's complement; ours are positive, so dropping leading zero
     bytes gives the magnitude Cryptokit expects. *)
  let integer (tlv : tlv) =
    if tlv.tag <> 0x02
    then Or_error.error_s [%message "DER: expected an integer"]
    else Ok (String.lstrip tlv.content ~drop:(fun c -> Char.equal c '\000'))
  ;;

  let encode_length n =
    if n < 0x80
    then String.of_char (Char.of_int_exn n)
    else (
      let rec bytes n acc =
        if n = 0 then acc else bytes (n lsr 8) (Char.of_int_exn (n land 0xff) :: acc)
      in
      let b = bytes n [] in
      String.of_char_list (Char.of_int_exn (0x80 lor List.length b) :: b))
  ;;

  let encode ~tag content =
    String.of_char (Char.of_int_exn tag) ^ encode_length (String.length content) ^ content
  ;;

  let encode_integer magnitude =
    let m = String.lstrip magnitude ~drop:(fun c -> Char.equal c '\000') in
    let m = if String.is_empty m then "\000" else m in
    encode ~tag:0x02 (if Char.to_int m.[0] >= 0x80 then "\000" ^ m else m)
  ;;

  (* AlgorithmIdentifier for rsaEncryption (OID 1.2.840.113549.1.1.1) with NULL
     parameters. *)
  let rsa_algorithm =
    encode ~tag:0x30 ("\x06\x09\x2a\x86\x48\x86\xf7\x0d\x01\x01\x01" ^ "\x05\x00")
  ;;
end

let bits_of_modulus n =
  let n = String.lstrip n ~drop:(fun c -> Char.equal c '\000') in
  if String.is_empty n
  then 0
  else (
    let top = Char.to_int n.[0] in
    let rec width v = if v = 0 then 0 else 1 + width (v lsr 1) in
    ((String.length n - 1) * 8) + width top)
;;

let base64 s = Mime.Base64.encode ~line_length:0 s

module Public_key = struct
  type t =
    { n : string
    ; e : string
    }

  let bits t = bits_of_modulus t.n
  let sexp_of_t t = [%sexp { rsa_public_key_bits = (bits t : int) }]

  let of_rsa_public_key der =
    let open Or_error.Let_syntax in
    match%bind Der.sequence der with
    | [ n; e ] ->
      let%bind n = Der.integer n in
      let%map e = Der.integer e in
      { n; e }
    | _ -> Or_error.error_s [%message "DER: not an RSAPublicKey"]
  ;;

  let of_der der =
    let open Or_error.Let_syntax in
    match%bind Der.sequence der with
    | [ { tag = 0x30; content = _ }; { tag = 0x03; content = bits } ]
      when String.length bits > 1 && Char.equal bits.[0] '\000' ->
      of_rsa_public_key (String.drop_prefix bits 1)
    | _ -> of_rsa_public_key der
  ;;

  let of_dns_record record =
    let tags =
      String.split record ~on:';'
      |> List.filter_map ~f:(fun tag ->
        match String.lsplit2 tag ~on:'=' with
        | None -> None
        | Some (name, value) -> Some (String.strip name, String.strip value))
    in
    match List.Assoc.find tags "p" ~equal:String.equal with
    | None -> Or_error.error_s [%message "DKIM record has no p= tag"]
    | Some "" -> Or_error.error_s [%message "DKIM key has been revoked (empty p=)"]
    | Some p -> Or_error.bind (Mime.Base64.decode p) ~f:of_der
  ;;

  let to_der t =
    let rsa = Der.encode ~tag:0x30 (Der.encode_integer t.n ^ Der.encode_integer t.e) in
    Der.encode ~tag:0x30 (Der.rsa_algorithm ^ Der.encode ~tag:0x03 ("\000" ^ rsa))
  ;;

  let to_dns_record t = "v=DKIM1; k=rsa; p=" ^ base64 (to_der t)

  let to_cryptokit t =
    { Cryptokit.RSA.size = bits t
    ; n = t.n
    ; e = t.e
    ; d = ""
    ; p = ""
    ; q = ""
    ; dp = ""
    ; dq = ""
    ; qinv = ""
    }
  ;;
end

module Private_key = struct
  type t = Cryptokit.RSA.key

  let bits (t : t) = t.size
  let sexp_of_t t = [%sexp "<redacted>", { rsa_private_key_bits = (bits t : int) }]
  let public_key (t : t) = { Public_key.n = t.n; e = t.e }

  let of_rsa_private_key der =
    let open Or_error.Let_syntax in
    match%bind Der.sequence der with
    | version :: n :: e :: d :: p :: q :: dp :: dq :: qinv :: _ ->
      let%bind version = Der.integer version in
      let%bind () =
        if String.is_empty version
        then Ok ()
        else Or_error.error_s [%message "RSA key: multi-prime keys are not supported"]
      in
      let%map n = Der.integer n
      and e = Der.integer e
      and d = Der.integer d
      and p = Der.integer p
      and q = Der.integer q
      and dp = Der.integer dp
      and dq = Der.integer dq
      and qinv = Der.integer qinv in
      { Cryptokit.RSA.size = bits_of_modulus n; n; e; d; p; q; dp; dq; qinv }
    | _ -> Or_error.error_s [%message "DER: not an RSAPrivateKey"]
  ;;

  let of_pkcs8 der =
    let open Or_error.Let_syntax in
    match%bind Der.sequence der with
    | _version
      :: { tag = 0x30; content = algorithm }
      :: { tag = 0x04; content = key }
      :: _ ->
      if String.is_prefix algorithm ~prefix:"\x06\x09\x2a\x86\x48\x86\xf7\x0d\x01\x01\x01"
      then of_rsa_private_key key
      else Or_error.error_s [%message "PKCS#8 key is not RSA"]
    | _ -> Or_error.error_s [%message "DER: not a PKCS#8 PrivateKeyInfo"]
  ;;

  let pem_body pem ~label =
    let begin_ = "-----BEGIN " ^ label ^ "-----" in
    let end_ = "-----END " ^ label ^ "-----" in
    match String.substr_index pem ~pattern:begin_ with
    | None -> None
    | Some start ->
      let rest = String.drop_prefix pem (start + String.length begin_) in
      Option.map (String.substr_index rest ~pattern:end_) ~f:(fun stop ->
        String.prefix rest stop)
  ;;

  let of_pem pem =
    let open Or_error.Let_syntax in
    let%bind key =
      match pem_body pem ~label:"PRIVATE KEY", pem_body pem ~label:"RSA PRIVATE KEY" with
      | Some body, _ -> Mime.Base64.decode body >>= of_pkcs8
      | None, Some body -> Mime.Base64.decode body >>= of_rsa_private_key
      | None, None ->
        Or_error.error_s
          [%message "expected an unencrypted PEM RSA key (BEGIN PRIVATE KEY)"]
    in
    if bits key < 1024
    then Or_error.error_s [%message "RSA key is too small" ~bits:(bits key : int)]
    else Ok key
  ;;
end

module Canonicalization = struct
  type t =
    | Simple
    | Relaxed
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Simple -> "simple"
    | Relaxed -> "relaxed"
  ;;

  let of_string = function
    | "simple" -> Some Simple
    | "relaxed" -> Some Relaxed
    | _ -> None
  ;;

  let is_wsp c = Char.equal c ' ' || Char.equal c '\t'

  (* Collapses runs of spaces and tabs into one space. *)
  let squeeze s =
    let buf = Buffer.create (String.length s) in
    let in_ws = ref false in
    String.iter s ~f:(fun c ->
      if is_wsp c
      then in_ws := true
      else (
        if !in_ws then Buffer.add_char buf ' ';
        in_ws := false;
        Buffer.add_char buf c));
    if !in_ws then Buffer.add_char buf ' ';
    Buffer.contents buf
  ;;

  let header t ~name ~value =
    match t with
    | Simple -> name ^ ":" ^ value ^ "\r\n"
    | Relaxed ->
      let value =
        String.substr_replace_all value ~pattern:"\r\n" ~with_:""
        |> squeeze
        |> String.strip ~drop:is_wsp
      in
      String.lowercase (String.rstrip name ~drop:is_wsp) ^ ":" ^ value ^ "\r\n"
  ;;

  let body t body =
    let lines = String.split body ~on:'\n' in
    let lines =
      List.map lines ~f:(fun l -> String.chop_suffix_if_exists l ~suffix:"\r")
    in
    let lines =
      match t with
      | Simple -> lines
      | Relaxed -> List.map lines ~f:(fun l -> String.rstrip ~drop:is_wsp (squeeze l))
    in
    let lines = List.rev (List.drop_while (List.rev lines) ~f:String.is_empty) in
    match lines, t with
    | [], Simple -> "\r\n"
    | [], Relaxed -> ""
    | lines, _ -> String.concat (List.map lines ~f:(fun l -> l ^ "\r\n"))
  ;;
end

module Config = struct
  type t =
    { domain : string
    ; selector : string
    ; key : Private_key.t
    ; header_canonicalization : Canonicalization.t
    ; body_canonicalization : Canonicalization.t
    ; headers : string list
    ; expire_after : Time_ns.Span.t option
    }
  [@@deriving sexp_of]

  let default_headers =
    [ "From"
    ; "Reply-To"
    ; "Subject"
    ; "Date"
    ; "To"
    ; "Cc"
    ; "Message-ID"
    ; "MIME-Version"
    ; "Content-Type"
    ; "Content-Transfer-Encoding"
    ; "List-Unsubscribe"
    ]
  ;;

  let create
    ?(header_canonicalization = Canonicalization.Relaxed)
    ?(body_canonicalization = Canonicalization.Relaxed)
    ?(headers = default_headers)
    ?expire_after
    ~domain
    ~selector
    key
    =
    { domain
    ; selector
    ; key
    ; header_canonicalization
    ; body_canonicalization
    ; headers
    ; expire_after
    }
  ;;
end

(* A header field as on the wire: name, and the value with its folding kept. *)
type field =
  { name : string
  ; value : string
  }

let split_message message =
  let head, body =
    match String.substr_index message ~pattern:"\r\n\r\n" with
    | Some i -> String.prefix message (i + 2), String.drop_prefix message (i + 4)
    | None -> message, ""
  in
  let lines =
    String.split (String.chop_suffix_if_exists head ~suffix:"\r\n") ~on:'\n'
    |> List.map ~f:(fun l -> String.chop_suffix_if_exists l ~suffix:"\r")
  in
  let fields =
    List.fold lines ~init:[] ~f:(fun acc line ->
      if String.length line > 0 && Canonicalization.is_wsp line.[0]
      then (
        match acc with
        | f :: rest -> { f with value = f.value ^ "\r\n" ^ line } :: rest
        | [] -> acc)
      else (
        match String.lsplit2 line ~on:':' with
        | Some (name, value) -> { name; value } :: acc
        | None -> acc))
    |> List.rev
  in
  fields, body
;;

(* For each name in [h=], the last not yet used field of that name (RFC 6376 section
   5.4.2); a name with no field left contributes nothing. *)
let select_fields fields names =
  let remaining = Hashtbl.create (module String) in
  List.iter (List.rev fields) ~f:(fun f ->
    Hashtbl.add_multi remaining ~key:(String.lowercase (String.strip f.name)) ~data:f);
  List.filter_map names ~f:(fun name ->
    let key = String.lowercase name in
    match Hashtbl.find remaining key with
    | Some (f :: rest) ->
      Hashtbl.set remaining ~key ~data:rest;
      Some f
    | _ -> None)
;;

(* EMSA-PKCS1-v1_5 encoding of a SHA-256 digest (RFC 8017 section 9.2). *)
let emsa_pkcs1_sha256 digest ~modulus_bytes =
  let digest_info =
    "\x30\x31\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20"
    ^ digest
  in
  let padding = modulus_bytes - String.length digest_info - 3 in
  "\x00\x01" ^ String.make padding '\xff' ^ "\x00" ^ digest_info
;;

let sha256 s = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s

let left_pad s ~len =
  if String.length s >= len then s else String.make (len - String.length s) '\000' ^ s
;;

let header_hash_input ~canon ~signed ~dkim_value =
  String.concat
    (List.map signed ~f:(fun f ->
       Canonicalization.header canon ~name:f.name ~value:f.value))
  ^ String.chop_suffix_exn
      (Canonicalization.header canon ~name:"DKIM-Signature" ~value:dkim_value)
      ~suffix:"\r\n"
;;

let fold_b b =
  let n = String.length b in
  let rec chunks i acc =
    if i >= n
    then List.rev acc
    else chunks (i + 72) (String.sub b ~pos:i ~len:(min 72 (n - i)) :: acc)
  in
  String.concat ~sep:"\r\n " (chunks 0 [])
;;

let sign (config : Config.t) ~now message =
  let fields, body = split_message message in
  if List.is_empty fields
  then Or_error.error_s [%message "DKIM: message has no header fields"]
  else (
    let present name =
      List.exists fields ~f:(fun f -> String.Caseless.equal (String.strip f.name) name)
    in
    let names = List.filter config.headers ~f:present in
    (* Listing From once more than it occurs means a From added later cannot be ignored by
       the verifier. *)
    let names = names @ [ "From" ] in
    let signed = select_fields fields names in
    let body_hash =
      base64 (sha256 (Canonicalization.body config.body_canonicalization body))
    in
    let timestamp =
      Int63.to_int_exn (Time_ns.to_int63_ns_since_epoch now) / 1_000_000_000
    in
    let tags =
      [ Some ("v", "1")
      ; Some ("a", "rsa-sha256")
      ; Some
          ( "c"
          , Canonicalization.to_string config.header_canonicalization
            ^ "/"
            ^ Canonicalization.to_string config.body_canonicalization )
      ; Some ("d", config.domain)
      ; Some ("s", config.selector)
      ; Some ("t", Int.to_string timestamp)
      ; Option.map config.expire_after ~f:(fun span ->
          "x", Int.to_string (timestamp + Float.iround_up_exn (Time_ns.Span.to_sec span)))
      ; Some ("h", String.concat ~sep:":" (List.map names ~f:String.lowercase))
      ; Some ("bh", body_hash)
      ]
      |> List.filter_opt
    in
    (* Fold between tags so that each line stays short; the folding is part of what simple
       header canonicalization signs, and it is written out unchanged below. *)
    let dkim_value =
      " "
      ^ String.concat ~sep:";\r\n " (List.map tags ~f:(fun (k, v) -> k ^ "=" ^ v))
      ^ ";\r\n b="
    in
    let input =
      header_hash_input ~canon:config.header_canonicalization ~signed ~dkim_value
    in
    let modulus_bytes = (Private_key.bits config.key + 7) / 8 in
    let em = emsa_pkcs1_sha256 (sha256 input) ~modulus_bytes in
    match Or_error.try_with (fun () -> Cryptokit.RSA.sign_CRT config.key em) with
    | Error e -> Error e
    | Ok signature ->
      let b = base64 (left_pad signature ~len:modulus_bytes) in
      Ok ("DKIM-Signature:" ^ dkim_value ^ fold_b b ^ "\r\n" ^ message))
;;

let parse_tags value =
  String.split value ~on:';'
  |> List.filter_map ~f:(fun tag ->
    match String.lsplit2 tag ~on:'=' with
    | None -> None
    | Some (k, v) ->
      Some (String.strip k, String.filter v ~f:(fun c -> not (Char.is_whitespace c))))
;;

(* The DKIM-Signature value with the b= tag's value removed, folding kept. *)
let without_b_value value =
  let n = String.length value in
  let rec find_b i at_tag_start =
    if i >= n
    then None
    else (
      let c = value.[i] in
      if at_tag_start && Char.equal c 'b' && i + 1 < n
      then (
        (* Skip whitespace between the tag name and '='. *)
        let rec eq j = if j < n && Char.is_whitespace value.[j] then eq (j + 1) else j in
        let j = eq (i + 1) in
        if j < n && Char.equal value.[j] '=' then Some (j + 1) else find_b (i + 1) false)
      else if Char.equal c ';'
      then find_b (i + 1) true
      else find_b (i + 1) (at_tag_start && Char.is_whitespace c))
  in
  match find_b 0 true with
  | None -> value
  | Some start ->
    let stop =
      match String.index_from value start ';' with
      | Some k -> k
      | None -> n
    in
    String.prefix value start ^ String.drop_prefix value stop
;;

let verify (key : Public_key.t) message =
  let open Or_error.Let_syntax in
  let fields, body = split_message message in
  let%bind dkim =
    match
      List.find fields ~f:(fun f ->
        String.Caseless.equal (String.strip f.name) "DKIM-Signature")
    with
    | Some f -> Ok f
    | None -> Or_error.error_s [%message "no DKIM-Signature field"]
  in
  let tags = parse_tags dkim.value in
  let tag name =
    match List.Assoc.find tags name ~equal:String.equal with
    | Some v -> Ok v
    | None -> Or_error.error_s [%message "DKIM-Signature lacks a tag" name]
  in
  let%bind algorithm = tag "a" in
  let%bind () =
    if String.equal algorithm "rsa-sha256"
    then Ok ()
    else Or_error.error_s [%message "unsupported algorithm" algorithm]
  in
  let%bind header_canon, body_canon =
    let c =
      Option.value (List.Assoc.find tags "c" ~equal:String.equal) ~default:"simple"
    in
    let h, b =
      match String.lsplit2 c ~on:'/' with
      | Some (h, b) -> h, b
      | None -> c, "simple"
    in
    match Canonicalization.of_string h, Canonicalization.of_string b with
    | Some h, Some b -> Ok (h, b)
    | _ -> Or_error.error_s [%message "unknown canonicalization" c]
  in
  let%bind bh = tag "bh" in
  let%bind () =
    if String.equal bh (base64 (sha256 (Canonicalization.body body_canon body)))
    then Ok ()
    else Or_error.error_s [%message "body hash does not match"]
  in
  let%bind h = tag "h" in
  let%bind b = tag "b" >>= Mime.Base64.decode in
  let others = List.filter fields ~f:(fun f -> not (phys_equal f dkim)) in
  let signed =
    select_fields others (String.split h ~on:':' |> List.map ~f:String.strip)
  in
  let input =
    header_hash_input ~canon:header_canon ~signed ~dkim_value:(without_b_value dkim.value)
  in
  let modulus_bytes = (Public_key.bits key + 7) / 8 in
  let%bind recovered =
    Or_error.try_with (fun () ->
      Cryptokit.RSA.unwrap_signature (Public_key.to_cryptokit key) b)
  in
  if String.equal
       (left_pad recovered ~len:modulus_bytes)
       (emsa_pkcs1_sha256 (sha256 input) ~modulus_bytes)
  then Ok ()
  else Or_error.error_s [%message "signature does not match"]
;;
