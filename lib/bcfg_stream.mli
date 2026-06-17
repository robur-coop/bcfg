(** Fully streaming, SAX-like API for [bcfg].

    This module lets one process a configuration as a flat sequence of
    {!type:lexeme}s, both for decoding (reading) and encoding (writing), without
    ever materialising the whole {!Bcfg_type.t} tree. The memory footprint is
    bounded by the nesting depth of the configuration, not by the size of the
    input. *)

type lexeme = Bcfg_type.Stream.lexeme =
  | Ds of string  (** start of a directive (with its name) *)
  | P of string  (** a parameter of the current directive *)
  | Os  (** opening of a children block ['{'] *)
  | Oe  (** closing of a children block ['}'] *)
  | De  (** end of the current directive *)

val pp_lexeme : Format.formatter -> lexeme -> unit

type error =
  [ `Lexer_error of
    Bcfg_txtloc.t * [ `Invalid_character of char | `Message of string ]
  | `Parser_error of Bcfg_txtloc.t * string ]

val pp_error : Format.formatter -> error -> unit

(** {1 Decoding.} *)

type decoder

val decoder : Lexing.lexbuf -> decoder
(** [decoder lexbuf] creates a streaming decoder pulling tokens from [lexbuf].
    Using [Lexing.from_channel] keeps the input bounded. *)

val decode : decoder -> (lexeme option, error) result
(** [decode d] returns the next lexeme. [Ok None] signals the end of the
    configuration; [Error _] a lexing or structural error, after which the
    decoder must not be used anymore. *)

val to_seq : Lexing.lexbuf -> (lexeme, error) result Seq.t
(** [to_seq lexbuf] is the lazy sequence of all lexemes (or the first error)
    pulled from [lexbuf]. *)

(** {1 Encoding.} *)

val encode :
  ?cfg:Bcfg_out.cfg -> (unit -> lexeme option) -> (string -> unit) -> unit
(** [encode ?cfg next sink] pulls lexemes from [next] and pushes the rendered
    chunks to [sink], producing the same output as {!Bcfg.emitter} would on the
    equivalent tree. *)

val to_string : ?cfg:Bcfg_out.cfg -> lexeme Seq.t -> string Seq.t
(** [to_string ?cfg seq] is the lazy sequence of rendered chunks for [seq]. *)

(** {1 Helpers.} *)

val of_t : Bcfg_type.t -> lexeme Seq.t
(** [of_t t] is the lexeme sequence equivalent to the tree [t]. *)

val to_t : lexeme Seq.t -> (Bcfg_type.t, error) result
(** [to_t seq] rebuilds the tree from a lexeme sequence. *)

val to_directives : Lexing.lexbuf -> (Bcfg_type.directive, error) result Seq.t
(** [to_directives lexbuf] yields the top-level directives one at a time,
    rebuilt from the lexeme stream. Only a single top-level subtree is held in
    memory at a time, so a whole (large) document is never materialised. *)
