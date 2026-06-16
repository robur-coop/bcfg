let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let rchop ~len str =
  if len > String.length str then invalid_arg "rchop";
  String.sub str 0 (String.length str - len)

let rchop_if_lf str =
  if str = "" then ""
  else
    match str.[String.length str - 1] with '\n' -> rchop ~len:1 str | _ -> str

let escape str =
  let buf = Buffer.create (String.length str) in
  let fn = function
    | '\x07' -> Buffer.add_string buf "\\a"
    | '\x08' -> Buffer.add_string buf "\\b"
    | '\x09' -> Buffer.add_string buf "\\t"
    | '\x0a' -> Buffer.add_string buf "\\n"
    | '\x0b' -> Buffer.add_string buf "\\v"
    | '\x0c' -> Buffer.add_string buf "\\f"
    | '\x0d' -> Buffer.add_string buf "\\r"
    | '\x20' .. '\x7e' as chr -> Buffer.add_char buf chr
    | chr ->
        let hex = Fmt.str "\\x%02x" (Char.code chr) in
        Buffer.add_string buf hex
  in
  String.iter fn str;
  Buffer.contents buf

let read_all ic =
  let buf = Buffer.create 0x10000 in
  let chunk = Bytes.create 0x10000 in
  let rec go () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0 then begin
      Buffer.add_substring buf (Bytes.unsafe_to_string chunk) 0 n;
      go ()
    end
  in
  go ();
  Buffer.contents buf

(* Read the whole input into memory once. We need the full content both to feed
   the parser and to re-read the lines around a potential error; buffering it in
   a string avoids the previous two-pipe [tee], which deadlocked on inputs
   larger than a pipe buffer (the error-context pipe was only drained on error).
   The input is read exactly once, so this also works on non-seekable stdin. *)
let read_input = function
  | None -> read_all stdin
  | Some filepath ->
      let ic = open_in_bin filepath in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> read_all ic)

let has_newline str =
  if str = "" then false else str.[String.length str - 1] = '\n'

let max_line_number lines =
  let fn acc (line_number, _) = max acc line_number in
  List.fold_left fn 0 lines

type cfg = { length_of_line_numbers : int }

let cfg lines =
  let v = max_line_number lines in
  let v = Float.of_int v |> Float.log10 |> Float.to_int |> succ in
  { length_of_line_numbers = v }

let pp_line_number ~cfg ppf line_number =
  let pp ppf line_number =
    Fmt.pf ppf "%*d" cfg.length_of_line_numbers line_number
  in
  Fmt.pf ppf "%a" Fmt.(styled `Faint pp) line_number

let pp_error ppf str =
  let str = escape str in
  Fmt.pf ppf "%a" Fmt.(styled (`Fg `Red) string) str

let pp_newline ppf = function
  | false -> ()
  | true -> Fmt.pf ppf "%a" Fmt.(styled `Faint string) "\\n"

let pp_line_with_ansi ~cfg ppf = function
  | line_number, Bcfg.Txtloc.Context_line line ->
      Fmt.pf ppf " %a %a@\n" (pp_line_number ~cfg) line_number
        Fmt.(styled `Faint string)
        (rchop_if_lf line)
  | line_number, Bcfg.Txtloc.Errored_line { err; around = pre, nxt } ->
      let with_newline = has_newline nxt in
      Fmt.pf ppf ">%a %s%a%s%a@\n" (pp_line_number ~cfg) line_number pre
        Fmt.(styled (`Fg `Red) pp_error)
        err (rchop_if_lf nxt) pp_newline with_newline
  | _, Eof { errored = true } ->
      Fmt.pf ppf ">%s %a@\n"
        (String.make cfg.length_of_line_numbers ' ')
        Fmt.(styled (`Fg `Red) string)
        "EOF"
  | _, Eof { errored = false } ->
      Fmt.pf ppf " %s %a@\n"
        (String.make cfg.length_of_line_numbers ' ')
        Fmt.(styled `Faint string)
        "EOF"

let pp_symbol : type a. a Bcfg.Error.symbol -> a Fmt.t =
 fun symbol ppf v ->
  match symbol with
  | Terminal Error -> Fmt.string ppf "#error"
  | Terminal Eof -> Fmt.string ppf "#eof"
  | Terminal Newline -> Fmt.string ppf "#newline"
  | Terminal Word -> Fmt.pf ppf "%S" v
  | Terminal LBrace -> Fmt.pf ppf "{"
  | Terminal RBrace -> Fmt.pf ppf "}"
  | Non_terminal Directives ->
      Fmt.pf ppf "@[<1>(Directives@ @[<hov>%a@])@]" Bcfg.pp_as_ocaml_value v
  | Non_terminal Newlines -> Fmt.string ppf "#newlines"
  | Non_terminal Directive ->
      Fmt.pf ppf "@[<1>(Directive @[<hov>%a@])@]" Bcfg.pp_as_ocaml_value [ v ]
  | Non_terminal Parameters ->
      Fmt.pf ppf "@[<hov>%a@]" Fmt.(Dump.list (fmt "%S")) v
  | Non_terminal Children ->
      Fmt.pf ppf "@[<1>(Children@ ]@[<hov>%a@])@]" Bcfg.pp_as_ocaml_value v
  | Non_terminal Top ->
      Fmt.pf ppf "@[<1>(Top@ @[<hov>%a@]@]" Bcfg.pp_as_ocaml_value v

let pp_error_message ppf (state, Bcfg.Error.Error (symbol, v)) =
  match (state, symbol) with
  | 1, Terminal Newline -> Fmt.pf ppf "> Unexpected closing curly bracked.@\n"
  | 4, Terminal Word ->
      Fmt.pf ppf
        "> The %a directive must end with a line break or an opening curly \
         bracket.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        v
  | 5, Terminal Word ->
      Fmt.pf ppf "> Unexpected closing curly bracket after %a.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        v
  | 8, Terminal Newline ->
      Fmt.pf ppf
        "> A directive (potentially with parameters) must always precede a \
         opening curly bracket.@\n"
  | 10, Terminal LBrace ->
      Fmt.pf ppf
        "> A opening curly bracket must always be followed by a line break.@\n"
  | 11, Non_terminal Newlines -> Fmt.pf ppf "> Missing a subdirective.@\n"
  | 12, Non_terminal Directives ->
      Fmt.pf ppf
        "> We are in a directive that does not end; a closing curly bracket \
         should follow the subdirective %a.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        (List.hd v).Bcfg.name
  | 13, Terminal RBrace ->
      Fmt.pf ppf
        "> A closing curly bracket must always be followed by a line break.@\n"
  | 15, Non_terminal Directive ->
      Fmt.pf ppf
        "> We are in a directive that does not end; a closing curly bracket \
         should follow the subdirective %a.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        v.Bcfg.name
  | 19, Non_terminal Directives ->
      Fmt.pf ppf
        "> The directive %a was never opened, so there is no reason to close \
         it.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        (List.hd v).Bcfg.name
  | 21, Non_terminal Directive ->
      Fmt.pf ppf
        "> The directive %a was never opened, so there is no reason to close \
         it.@\n"
        Fmt.(styled (`Fg `Yellow) (fmt "%S"))
        v.Bcfg.name
  | state, symbol ->
      Fmt.pf ppf "%d => @[<hov>%a@]@\n" state (pp_symbol symbol) v

let pp_error_with_source ~content ppf err =
  let lines txtloc = Bcfg.Txtloc.lines_around_txtloc_string ~txtloc content in
  match err with
  | `Lexer_error (txtloc, `Message msg) ->
      let lines = lines txtloc in
      let cfg = cfg lines in
      Fmt.pf ppf "Error at %a:@\n" Bcfg.Txtloc.pp txtloc;
      (* NOTE(dinosaure): [msg] (from [Lexer]) does **not** contains ['\n']. *)
      Fmt.pf ppf "> %s@\n" msg;
      let fn = Fmt.pf ppf "%a" (pp_line_with_ansi ~cfg) in
      List.iter fn lines
  | `Lexer_error (txtloc, `Invalid_character chr) ->
      let lines = lines txtloc in
      let cfg = cfg lines in
      Fmt.pf ppf "Invalid character %S at %a:@\n" (String.make 1 chr)
        Bcfg.Txtloc.pp txtloc;
      let fn = Fmt.pf ppf "%a" (pp_line_with_ansi ~cfg) in
      List.iter fn lines
  | `Parser_error (txtloc, None) ->
      let lines = lines txtloc in
      let cfg = cfg lines in
      Fmt.pf ppf "Invalid syntax at %a:@\n" Bcfg.Txtloc.pp txtloc;
      let fn = Fmt.pf ppf "%a" (pp_line_with_ansi ~cfg) in
      List.iter fn lines
  | `Parser_error (txtloc, Some err) ->
      let lines = lines txtloc in
      let cfg = cfg lines in
      Fmt.pf ppf "Error at %a:@\n" Bcfg.Txtloc.pp txtloc;
      (* NOTE(dinosaure): [msg] already contains ['\n']. *)
      Fmt.pf ppf "%a" pp_error_message err;
      let fn = Fmt.pf ppf "%a" (pp_line_with_ansi ~cfg) in
      List.iter fn lines
  | `Rejected -> Fmt.pf ppf "Rejected@\n"

let run quiet input =
  let content = read_input input in
  let lexbuf = Lexing.from_string ~with_positions:true content in
  match Bcfg.parser lexbuf with
  | Ok _ -> Ok 0
  | Error err when not quiet ->
      Fmt.epr "%a%!" (pp_error_with_source ~content) err;
      Ok 1
  | Error _ -> Ok 1

open Cmdliner
open Bcfg_cli

let input =
  let doc = "The configuration file to validate." in
  let parser str =
    match str with
    | "-" -> Ok None
    | filepath when Sys.file_exists filepath && is_regular_file filepath ->
        Ok (Some filepath)
    | filepath ->
        error_msgf "%S does not exist or is not a regular file" filepath
  in
  let pp ppf = function
    | None -> Fmt.string ppf "-"
    | Some filepath -> Fmt.string ppf filepath
  in
  let input = Arg.conv (parser, pp) in
  let open Arg in
  value & pos 0 input None & info [] ~doc ~docv:"FILE"

let term =
  let open Term in
  const run $ setup_logs $ input |> term_result ~usage:false

let cmd =
  let doc = "$(tname) validates a configuration file." in
  let man = [] in
  let info = Cmd.info "validate" ~doc ~man in
  Cmd.v info term
