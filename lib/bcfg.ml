(* Copyright (c) 2022 Léo Andrès
   SPDX-License-Identifier: ISC
   Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
   SPDX-License-Identifier: ISC

   About [Fmt]:
   Copyright (c) 2016 The fmt programmers
   SPDX-License-Identifier: ISC *)

module Txtloc = Bcfg_txtloc
module Out = Bcfg_out
module M = Bcfg_parser.MenhirInterpreter

let unescape = Bcfg_lexer.unescape

module Fmt = struct
  let box ?(ident = 0) pp_value ppf value =
    let open Format in
    pp_open_box ppf ident;
    pp_value ppf value;
    pp_close_box ppf ()

  let surround s0 s1 pp_value ppf value =
    let open Format in
    pp_print_string ppf s0;
    pp_value ppf value;
    pp_print_string ppf s1

  let brackets pp_value = box ~ident:1 (surround "[" "]" pp_value)
  let cut ppf _ = Format.pp_print_cut ppf ()

  let iter ?(sep = cut) iter pp_value ppf value =
    let is_first = ref true in
    let pp_value value =
      if !is_first then is_first := false else sep ppf ();
      pp_value ppf value
    in
    iter pp_value value

  let sp ppf _ = Format.pp_print_space ppf ()

  let semi ppf _ =
    Format.pp_print_string ppf ";";
    sp ppf ()

  let list ?sep pp_value = iter ?sep List.iter pp_value
  let flist pp_value = brackets (list ~sep:semi (box pp_value))
  let pf = Format.fprintf
  let fmt fmt ppf = pf ppf fmt
end

type directive = Bcfg_type.directive = {
  name : string;
  parameters : string list;
  children : directive list;
}

type t = Bcfg_type.t

let rec pp_directive ppf { name; parameters; children } =
  Fmt.pf ppf
    "@[<2>{ @[<hov>name=@ %S;@ parameters=@ @[<hov>%a@];@ children=@ \
     @[<hov>%a@]@] }@]"
    name
    Fmt.(flist (fmt "%S"))
    parameters
    Fmt.(flist pp_directive)
    children

let pp_as_ocaml_value ppf t = Fmt.list pp_directive ppf t

module Stream = Bcfg_stream

module Error = struct
  type state = int

  type 'a terminal =
    | Error : unit terminal
    | Word : string terminal
    | RBrace : unit terminal
    | LBrace : unit terminal
    | Newline : unit terminal
    | Eof : unit terminal

  type 'a non_terminal =
    | Directives : directive list non_terminal
    | Newlines : unit non_terminal
    | Parameters : string list non_terminal
    | Directive : directive non_terminal
    | Children : directive list non_terminal
    | Top : t non_terminal

  type 'a symbol =
    | Terminal : 'a terminal -> 'a symbol
    | Non_terminal : 'a non_terminal -> 'a symbol

  type t = Error : 'a symbol * 'a -> t

  let from_symbol : type a. a M.symbol -> a -> t =
   fun symbol v ->
    match symbol with
    | M.T M.T_error -> Error (Terminal Error, ())
    | M.T M.T_WORD -> Error (Terminal Word, v)
    | M.T M.T_RBRACE -> Error (Terminal RBrace, ())
    | M.T M.T_LBRACE -> Error (Terminal LBrace, ())
    | M.T M.T_EOF -> Error (Terminal Eof, ())
    | M.T M.T_NEWLINE -> Error (Terminal Newline, ())
    (* | M.N M.N_nonempty_list_directive_ -> Error (Non_terminal Directives, v) *)
    | M.N M.N_list_directive_ -> Error (Non_terminal Directives, v)
    | M.N M.N_config -> Error (Non_terminal Top, v)
    | M.N M.N_nonempty_list_NEWLINE_ -> Error (Non_terminal Newlines, ())
    | M.N M.N_list_NEWLINE_ -> Error (Non_terminal Newlines, ())
    | M.N M.N_directive -> Error (Non_terminal Directive, v)
    | M.N M.N_children -> Error (Non_terminal Children, v)
    | M.N M.N_list_WORD_ -> Error (Non_terminal Parameters, v)

  let from_env env =
    match M.stack env with
    | (lazy Nil) -> None
    | (lazy (Cons (M.Element (state, v, _, _), _))) ->
        let symbol = from_symbol (M.incoming_symbol state) v in
        let state = M.number state in
        Some (state, symbol)
end

type error =
  [ `Lexer_error of
    Bcfg_txtloc.t * [ `Invalid_character of char | `Message of string ]
  | `Parser_error of Bcfg_txtloc.t * (Error.state * Error.t) option
  | `Rejected ]

let pp_error_for_human ppf = function
  | `Lexer_error (_, `Invalid_character chr) ->
      Fmt.pf ppf "Invalid character %S" (String.make 1 chr)
  | `Lexer_error (_, `Message msg) -> Fmt.pf ppf "%s" msg
  | `Parser_error _ -> Fmt.pf ppf "Parser error"
  | `Rejected -> Fmt.pf ppf "Rejected"

let rec parser lexbuf (checkpoint : t M.checkpoint) =
  match checkpoint with
  | M.InputNeeded _env ->
      let token = Bcfg_lexer.token lexbuf in
      let a = lexbuf.Lexing.lex_start_p in
      let b = lexbuf.Lexing.lex_curr_p in
      let checkpoint = M.offer checkpoint (token, a, b) in
      parser lexbuf checkpoint
  | M.Shifting _ | M.AboutToReduce _ ->
      let checkpoint = M.resume checkpoint in
      parser lexbuf checkpoint
  | M.HandlingError env ->
      let txtloc = Bcfg_txtloc.from_lexbuf lexbuf in
      let err = Error.from_env env in
      Error (`Parser_error (txtloc, err))
  | M.Accepted v -> Ok v
  | M.Rejected -> Error `Rejected

let parser lexbuf =
  try parser lexbuf (Bcfg_parser.Incremental.config lexbuf.lex_curr_p)
  with Bcfg_lexer.Unexpected_character chr ->
    let txtloc = Bcfg_txtloc.from_lexbuf lexbuf in
    Error (`Lexer_error (txtloc, `Invalid_character chr))

let emitter ?(cfg = Out.config ()) t =
  let w = Out.emitter ~cfg t (Out.Writer.ctx ()) in
  Out.Writer.to_seq w
