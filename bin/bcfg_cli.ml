open Cmdliner

let src = Logs.Src.create "bcfg.cli"

module Log = (val Logs.src_log src : Logs.LOG)

let ( <.> ) f g = fun x -> f (g x)
let msg msg = `Msg msg
let msgf fmt = Fmt.kstr msg fmt
let error_msgf fmt = Fmt.kstr (Result.error <.> msg) fmt
let is_regular_file filepath = Sys.is_directory filepath = false

(* Logs *)

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BCFG_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "BCFG_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow $(tname) to emit UTF-8 characters." in
  let env = Cmd.Env.info "BCFG_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

(* Output configuration *)

let output_configuration_options = "OUTPUT CONFIGURATION OPTIONS"

let margin =
  let doc = "Specify a limit when we output a $(b,bcfg) value." in
  let open Arg in
  value & opt int 80
  & info [ "margin" ] ~doc ~docv:"COLUMN" ~docs:output_configuration_options

let indent =
  let doc =
    "How to indent subdirectives when outputting a $(b,bcfg) value: a number \
     of spaces per level, or $(b,tab) (also $(b,\\\\t)) to indent with tab \
     characters. This is in the spirit of $(b,vim)'s $(b,expandtab) and \
     $(b,shiftwidth)."
  in
  let parser str =
    match String.lowercase_ascii str with
    | "tab" | "t" | "\t" | "\\t" -> Ok `Tab
    | _ -> (
        match int_of_string_opt str with
        | Some n when n >= 0 -> Ok (`Spaces n)
        | _ ->
            Error
              (msgf "Invalid indentation %S (expected a number or \"tab\")" str)
        )
  in
  let pp ppf = function
    | `Tab -> Fmt.string ppf "tab"
    | `Spaces n -> Fmt.int ppf n
  in
  let indent_conv = Arg.conv (parser, pp) in
  let open Arg in
  value
  & opt indent_conv (`Spaces 2)
  & info [ "indent" ] ~doc ~docv:"WIDTH" ~docs:output_configuration_options

let tab_width =
  let doc =
    "The visual width of a tab (its $(b,tabstop)), used for margin \
     computations when indenting with tabs."
  in
  let open Arg in
  value & opt int 8
  & info [ "tab-width" ] ~doc ~docv:"NUMBER" ~docs:output_configuration_options

let escape =
  let open Arg in
  let all_with_hex =
    let doc = "Escape all non-UTF-8 characters via their hexadecimal values." in
    ( `All_with_hex,
      info [ "escape-with-hex" ] ~doc ~docs:output_configuration_options )
  in
  value & vflag `Normal [ all_with_hex ]

let hex =
  let open Arg in
  let lower =
    let doc = "Use lowercase alphabet to output hexadecimal values." in
    (`Lower, info [ "hex-lower" ] ~doc ~docs:output_configuration_options)
  in
  let upper =
    let doc = "Use uppercase alphabet to oputput hexadecimal values." in
    (`Upper, info [ "hex-upper" ] ~doc ~docs:output_configuration_options)
  in
  value & vflag `Lower [ lower; upper ]

let setup_output_configuration margin indent tab_width escape hex =
  match indent with
  | `Spaces n -> Bcfg.Out.config ~margin ~indent:n ~tab:false ~escape ~hex ()
  | `Tab -> Bcfg.Out.config ~margin ~indent:tab_width ~tab:true ~escape ~hex ()

let setup_output_configuration =
  let open Term in
  const setup_output_configuration $ margin $ indent $ tab_width $ escape $ hex
