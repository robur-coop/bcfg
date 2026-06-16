module Writer : sig
  type t
  type ctx

  val ctx : unit -> ctx
  val column : ctx -> int
  val line : ctx -> int
  val ( let* ) : t -> (unit -> t) -> t
  val flush : ctx -> t
  val newline : ctx -> t
  val write : string -> ctx -> t
  val writev : string list -> ctx -> t
  val of_seq : string Seq.t -> ctx -> t
  val to_seq : t -> string Seq.t
end

type cfg

val config :
  ?margin:int ->
  ?indent:int ->
  ?tab:bool ->
  ?escape:[ `All_with_hex | `Normal ] ->
  ?hex:[ `Lower | `Upper ] ->
  unit ->
  cfg

val emitter : cfg:cfg -> Bcfg_type.t -> Writer.ctx -> Writer.t

val lexeme_emitter :
  cfg:cfg -> (unit -> Bcfg_type.Stream.lexeme option) -> Writer.ctx -> Writer.t
(** [lexeme_emitter ~cfg next ctx] is a streaming emitter which pulls lexemes
    from [next] (until it returns [None]) and produces the same output as
    {!emitter} would on the equivalent {!Bcfg_type.t}. The memory footprint is
    bounded by the nesting depth of the configuration. *)
