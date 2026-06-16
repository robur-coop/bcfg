let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let pp_error ppf = function
  | `Lexer_error (loc, `Invalid_character chr) ->
      Fmt.pf ppf "Invalid character %C at %a" chr Bcfg.Txtloc.pp loc
  | `Lexer_error (loc, `Message msg) ->
      Fmt.pf ppf "%s at %a" msg Bcfg.Txtloc.pp loc
  | `Parser_error (loc, _) ->
      Fmt.pf ppf "Invalid syntax at %a" Bcfg.Txtloc.pp loc
  | `Rejected -> Fmt.pf ppf "Rejected configuration"

let read_and_parse input =
  let ic, finally =
    match input with
    | None -> (stdin, ignore)
    | Some filepath ->
        let ic = open_in_bin filepath in
        (ic, fun () -> close_in ic)
  in
  let@ () = finally in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Bcfg.parser lexbuf

let run _quiet cfg in_place input =
  match (in_place, input) with
  | true, None ->
      error_msgf "Cannot use $(b,--in-place) when reading from standard input."
  | _ -> (
      match read_and_parse input with
      | Error err ->
          Fmt.epr "%a: %a.@."
            Fmt.(option ~none:(any "<stdin>") string)
            input pp_error err;
          Fmt.epr "Run $(b,bcfg validate) for a detailed report.@.";
          Ok 1
      | Ok v ->
          let seq = Bcfg.emitter ~cfg v in
          (match (in_place, input) with
          | true, Some filepath ->
              let oc = open_out_bin filepath in
              let@ () = fun () -> close_out oc in
              Seq.iter (output_string oc) seq
          | _ -> Seq.iter (output_string stdout) seq);
          Ok 0)

open Cmdliner
open Bcfg_cli

let input =
  let doc = "The configuration file to lint ($(b,-) for standard input)." in
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

let in_place =
  let doc = "Rewrite the given file in place instead of printing to stdout." in
  let open Arg in
  value & flag & info [ "i"; "in-place" ] ~doc

let term =
  let open Term in
  const run $ setup_logs $ setup_output_configuration $ in_place $ input
  |> term_result ~usage:false

let cmd =
  let doc = "$(tname) reformats (pretty-prints) a configuration file." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) parses a $(b,bcfg) configuration and prints it back in a \
         canonical form: it re-indents the directives and wraps long values \
         that would otherwise exceed the right margin.";
      `P
        "The indentation width and the margin (the column at which values are \
         wrapped) are configurable, in the spirit of $(b,vim)'s \
         $(b,shiftwidth) and $(b,textwidth):";
      `Pre "> bcfg lint --indent 4 --margin 100 file.cfg";
      `P
        "By default the result is written to standard output; use \
         $(b,--in-place) (or $(b,-i)) to overwrite the input file.";
    ]
  in
  let info = Cmd.info "lint" ~doc ~man in
  Cmd.v info term
