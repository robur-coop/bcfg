type t =
  [ `Null
  | `Bool of bool
  | `String of string
  | `List of t list
  | `Assoc of (string * t) list ]

let params_value = function
  | [] -> `Null
  | [ x ] -> `String x
  | xs -> `List (List.map (fun s -> `String s) xs)

let rec value_of_directive (d : Bcfg.directive) : t =
  match d.Bcfg.children with
  | [] -> params_value d.Bcfg.parameters
  | children ->
      let entries = assoc_of_directives children in
      let entries =
        match d.Bcfg.parameters with
        | [] -> entries
        | params -> ("$params", params_value params) :: entries
      in
      `Assoc entries

(* Group directives by name, preserving first-occurrence order; names appearing
   more than once are collected into an array. *)
and assoc_of_directives (ds : Bcfg.t) : (string * t) list =
  let order = ref [] in
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (d : Bcfg.directive) ->
      let v = value_of_directive d in
      match Hashtbl.find_opt tbl d.Bcfg.name with
      | None ->
          order := d.Bcfg.name :: !order;
          Hashtbl.replace tbl d.Bcfg.name [ v ]
      | Some vs -> Hashtbl.replace tbl d.Bcfg.name (v :: vs))
    ds;
  List.rev_map
    (fun name ->
      match Hashtbl.find tbl name with
      | [ v ] -> (name, v)
      | vs -> (name, `List (List.rev vs)))
    !order

let of_config (ds : Bcfg.t) : t = `Assoc (assoc_of_directives ds)

let escape_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let to_string ?(minify = false) (json : t) =
  let buf = Buffer.create 256 in
  let nl indent =
    if not minify then begin
      Buffer.add_char buf '\n';
      Buffer.add_string buf (String.make (indent * 2) ' ')
    end
  in
  let colon = if minify then ":" else ": " in
  let rec go indent = function
    | `Null -> Buffer.add_string buf "null"
    | `Bool b -> Buffer.add_string buf (string_of_bool b)
    | `String s -> escape_string buf s
    | `List [] -> Buffer.add_string buf "[]"
    | `List items ->
        Buffer.add_char buf '[';
        List.iteri
          (fun i item ->
            if i > 0 then Buffer.add_char buf ',';
            nl (indent + 1);
            go (indent + 1) item)
          items;
        nl indent;
        Buffer.add_char buf ']'
    | `Assoc [] -> Buffer.add_string buf "{}"
    | `Assoc fields ->
        Buffer.add_char buf '{';
        List.iteri
          (fun i (k, v) ->
            if i > 0 then Buffer.add_char buf ',';
            nl (indent + 1);
            escape_string buf k;
            Buffer.add_string buf colon;
            go (indent + 1) v)
          fields;
        nl indent;
        Buffer.add_char buf '}'
  in
  go 0 json;
  Buffer.contents buf

let pp ppf json = Format.pp_print_string ppf (to_string json)
