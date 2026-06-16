let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind

let run _quiet cfg output_format query input =
  let* query = Bcfgq.of_string query in
  let ic, finally =
    match input with
    | None -> (stdin, ignore)
    | Some filepath ->
        let ic = open_in_bin filepath in
        (ic, fun () -> close_in ic)
  in
  let@ () = finally in
  let lexbuf = Lexing.from_channel ic in
  let* bcfg = Bcfg.parser lexbuf in
  let result = Bcfgq.eval query bcfg in
  (match output_format with
  | `Bcfg -> Seq.iter (output_string stdout) (Bcfg.emitter ~cfg result)
  | `Json ->
      print_string (Bcfg_json.to_string (Bcfg_json.of_config result));
      print_newline ());
  Ok 0

let to_msg = function
  | `Msg _ as m -> m
  | #Bcfg.error -> `Msg "Invalid bcfg file"

open Cmdliner
open Bcfg_cli

let input =
  let doc = "The configuration file to query ($(b,-) for standard input)." in
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
  value & pos 1 input None & info [] ~doc ~docv:"FILE"

let query =
  let doc = "The $(b,bcfg) query." in
  let open Arg in
  required & pos 0 (some string) None & info [] ~doc ~docv:"QUERY"

let output_format =
  let doc = "The output format of the query result ($(b,bcfg) or $(b,json))." in
  let open Arg in
  value
  & opt (enum [ ("bcfg", `Bcfg); ("json", `Json) ]) `Bcfg
  & info [ "o"; "output-format" ] ~doc ~docv:"FORMAT"

let term =
  let open Term in
  const run $ setup_logs $ setup_output_configuration $ output_format $ query
  $ input
  |> map (Result.map_error to_msg)
  |> term_result ~usage:false

let cmd =
  let doc =
    "$(tname) applies a query to the given $(b,bcfg) configuration file."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) selects directives from a $(b,bcfg) configuration using a \
         small query language and prints them back, either as $(b,bcfg) or as \
         JSON (to be piped into tools such as $(b,jq)).";
      `S "QUERY LANGUAGE";
      `P "$(b,foo) selects the top-level directives named \"foo\".";
      `P "$(b,foo.bar) selects the \"bar\" sub-directives of \"foo\".";
      `P "$(b,foo[0]) keeps the first parameter of the matching directives.";
      `P
        "$(b,foo\\(a|b\\)) keeps the \"foo\" directives having a parameter \
         matching \"a\" or \"b\".";
    ]
  in
  let info = Cmd.info "query" ~doc ~man in
  Cmd.v info term
