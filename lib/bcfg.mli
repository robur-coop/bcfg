type directive = Bcfg_type.directive = {
  name : string;
  parameters : string list;
  children : directive list;
}

type t = Bcfg_type.t

(* [Txtloc] allows you to locate errors that may occur when manipulating a
   [bcfg] file. You can then contextualize the error and display it using a list
   of {!type:line}. *)
module Txtloc : sig
  type t

  val pp : Format.formatter -> t -> unit

  type line =
    | Errored_line of { err : string; around : string * string }
    | Context_line of string
    | Eof of { errored : bool }

  val lines_around_txtloc :
    ?ctx:int -> txtloc:t -> in_channel -> (int * line) list
  (** If the error is located in [txtloc], [lines_around_txtloc ~txtloc ic]
      contextualizes the error based on the file content available via [ic].
      Note that the [ic] used {b must not} be the same as the one used (and
      already consumed) to analyze the [bcfg] file. *)
end

(** {1 Configuration file parser.}

    The parser offered by [bcfg] comes from [menhir]. It allows us to implement
    a parser according to a grammatical specification. The advantage of [menhir]
    lies in its ability to introspect syntax errors that may occur during
    analysis. In this case, parse can return a fairly extensive error allowing
    you to contextualize it.

    This contextualization of errors requires some knowledge of LR(1) parsers.
    We recommend reading the [menhir] manual before working with the
    {!type:Error.t} type in order to understand how to contextualize a syntax
    error. *)

module Error : sig
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
end

type error =
  [ `Lexer_error of
    Txtloc.t * [ `Invalid_character of char | `Message of string ]
  | `Parser_error of Txtloc.t * (Error.state * Error.t) option
  | `Rejected ]

val pp_error_for_human : Format.formatter -> error -> unit
val parser : Lexing.lexbuf -> (t, [> error ]) result

module Out : sig
  type cfg

  val config :
    ?margin:int ->
    ?indent:int ->
    ?tab:bool ->
    ?escape:[ `All_with_hex | `Normal ] ->
    ?hex:[ `Lower | `Upper ] ->
    unit ->
    cfg
end

val emitter : ?cfg:Out.cfg -> t -> string Seq.t
val pp_as_ocaml_value : Format.formatter -> t -> unit

module Stream = Bcfg_stream
(** Fully streaming, SAX-like API. See {!Bcfg_stream}. *)

(**/**)

val unescape : string -> string
