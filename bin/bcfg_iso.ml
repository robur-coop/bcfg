let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let function_from_seq seq =
  let cell = ref (None, seq) in
  let consume_remaining buf max = function
    | Some (str, off) ->
        let len = Int.min max (String.length str - off) in
        Bytes.blit_string str off buf 0 len;
        if len = String.length str - off then Some (None, len)
        else Some (Some (str, off + len), len)
    | None -> None
  in
  let rec consume buf max =
    let remaining, seq = !cell in
    match consume_remaining buf max remaining with
    | Some (remaining, len) ->
        cell := (remaining, seq);
        len
    | None ->
        begin match Seq.uncons seq with
        | None -> 0
        | Some (str, seq) ->
            cell := (Some (str, 0), seq);
            consume buf max
        end
  in
  consume

let output_or_return ~cfg v = function
  | None -> ()
  | Some None ->
      let seq = Bcfg.emitter ~cfg v in
      Seq.iter (output_string stdout) seq
  | Some (Some filepath) ->
      let seq = Bcfg.emitter ~cfg v in
      let oc = open_out_bin filepath in
      let@ () = fun () -> close_out oc in
      Seq.iter (output_string oc) seq

let run _quiet cfg input output =
  let ic, finally =
    match input with
    | None -> (stdin, ignore)
    | Some filepath ->
        let ic = open_in_bin filepath in
        let finally () = close_in ic in
        (ic, finally)
  in
  let@ () = finally in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  match Bcfg.parser lexbuf with
  | Error _ -> error_msgf "Invalid bcfg file."
  | Ok v -> begin
      let seq = Bcfg.emitter ~cfg v in
      let fn = function_from_seq seq in
      let lexbuf = Lexing.from_function fn in
      match Bcfg.parser lexbuf with
      | Ok v' when v = v' ->
          output_or_return ~cfg v output;
          Ok 0
      | _ -> Ok 1
    end

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

let output =
  let doc =
    "Outputs the configuration file reconstructed by $(tname) (to a file or to \
     standard output)."
  in
  let parser str =
    match str with
    | "-" -> Ok None
    | filepath when not (Sys.file_exists filepath) -> Ok (Some filepath)
    | filepath -> error_msgf "%S already exists" filepath
  in
  let pp ppf = function
    | None -> Fmt.string ppf "-"
    | Some filepath -> Fmt.string ppf filepath
  in
  let open Arg in
  value & pos 1 (some (conv (parser, pp))) None & info [] ~doc ~docv:"FILE"

let term =
  let open Term in
  const run $ setup_logs $ setup_output_configuration $ input $ output
  |> term_result ~usage:false

let cmd =
  let doc = "$(tname) verifies our isomorphism assumption on the given file." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a tool for verifying a property that must always be true \
         for all $(b,bcfg) documents: isomorphism. That is, if $(b,bcfg) is \
         capable of parsing a configuration file, it must also be able to \
         re-output it without altering the information. In other words:";
      `Pre "> cfg = decode(encode(cfg))";
      `P
        "If this property is not respected by $(b,bcfg), i.e. if this command \
         returns [1]:";
      `Pre "> $(b,bcfg) $(tname) file.cfg\n> [1]";
      `P "Well done! You have found a bug.";
    ]
  in
  let info = Cmd.info "iso" ~doc ~man in
  Cmd.v info term
