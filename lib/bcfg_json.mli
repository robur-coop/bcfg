(** A small, dependency-free JSON view of a [bcfg] configuration, useful to pipe
    a query result into tools such as [jq].

    {2 Mapping}

    A configuration (a list of directives) becomes a JSON object mapping every
    directive name to its {e value}. The value of a directive is:
    - [null] when it has no parameter and no children;
    - the parameter (a string) when it has exactly one parameter and no
      children;
    - the array of its parameters when it has several and no children;
    - an object built from its children otherwise, with its parameters, if any,
      kept under the ["$params"] key.

    Directives sharing the same name (e.g. repeated entries) are grouped into a
    JSON array, in their order of appearance. *)

type t =
  [ `Null
  | `Bool of bool
  | `String of string
  | `List of t list
  | `Assoc of (string * t) list ]

val of_config : Bcfg.t -> t
val to_string : ?minify:bool -> t -> string
val pp : Format.formatter -> t -> unit
