(** A [jsont]-like, bidirectional combinator API to read and write OCaml values
    from/to [bcfg] configurations.

    A value of type ['a t] is a {b codec}: it knows both how to {!decode} a
    [bcfg] fragment into an ['a] and how to {!encode} an ['a] back into [bcfg].

    Two locations carry values in [bcfg], and they map to the two main kinds of
    codecs:
    - {b scalars} ({!string}, {!int}, {!bool}, {!float}, ...) are read from a
      positional parameter (see {!req}) or from the first parameter of a named
      sub-directive (see {!field});
    - {b directives} (records), built with {!directive}, {!req}, {!field} and
      {!opt}, then closed with {!uniq} (exactly one) or {!some} (a list). *)

type 'a t
(** The type of codecs for values of type ['a]. *)

type ('r, 'fn) directive
(** A record codec under construction, building a value of type ['r] with the
    constructor ['fn]. *)

type error = [ `Msg of string ]

(** {1 Scalars.} *)

val string : string t
val int : int t
val bool : bool t
val float : float t

val map : dec:('a -> 'b) -> enc:('b -> 'a) -> 'a t -> 'b t
(** [map ~dec ~enc t] transforms the codec [t] with [dec] (when decoding) and
    [enc] (when encoding). *)

val enum : (string * 'a) list -> 'a t
(** [enum cases] is a scalar codec mapping each string to its associated value
    and back. *)

(** {1 Cardinality.} *)

val list : 'a t -> 'a list t
(** [list t], used as a {!field} type, collects every sub-directive of that name
    (zero or more). *)

val option : 'a t -> 'a option t
(** [option t], used as a {!field} type, makes the field optional (zero or one).
    Equivalent to using {!opt}. *)

(** {1 Records (directives).} *)

val directive :
  ?name:string -> ?documentation:string -> 'fn -> ('a, 'fn) directive
(** [directive ?name fn] starts a record codec applying the collected values to
    [fn]. [name], when given, is the directive keyword (used for filtering when
    decoding and as the keyword when encoding at the top-level). The parameters
    and fields must be declared in the same order as the arguments of [fn]. *)

val req :
  ?pos:int ->
  ?documentation:string ->
  'a t ->
  ('r -> 'a) ->
  ('r, 'a -> 'v) directive ->
  ('r, 'v) directive
(** [req ?pos t get d] adds a positional parameter decoded with [t]. [pos] is
    its index (defaulting to the number of parameters declared so far). [get]
    projects it back when encoding. *)

val field :
  string ->
  ?documentation:string ->
  'a t ->
  ('r -> 'a) ->
  ('r, 'a -> 'v) directive ->
  ('r, 'v) directive
(** [field name t get d] adds a required sub-directive named [name] decoded with
    [t]. *)

val opt :
  string ->
  ?documentation:string ->
  'a t ->
  ?get:('r -> 'a option) ->
  ('r, 'a option -> 'v) directive ->
  ('r, 'v) directive
(** [opt name t d] adds an optional sub-directive named [name]. *)

val some : ('a, 'a) directive -> 'a list t
(** [some d] is the codec of a (possibly empty) list of [d] directives. *)

val uniq : ('a, 'a) directive -> 'a t
(** [uniq d] is the codec of exactly one [d] directive. It can also be used as a
    {!field} type for nested records. *)

(** {1 Recursion.} *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] ties a recursive codec, e.g. for a tree-shaped configuration. *)

(** {1 Decoding and encoding.} *)

val decode : 'a t -> Bcfg.t -> ('a, [> error ]) result
val encode : 'a t -> 'a -> Bcfg.t
