module Bcfg_query = Bcfg_query

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

(* An index ([foo[0]]) is lexed as a plain word: it is up to us to check that
   it is a valid number. The walk rejects the query upfront so that {!eval}
   never has to deal with an invalid (or overflowing) index. *)
let rec validate_pattern =
  let open Bcfg_query in
  function
  | PWord _ | PAny -> Ok ()
  | PEval e -> validate_expr e
  | PNot p -> validate_pattern p
  | PAnd (a, b) | POr (a, b) ->
      Result.bind (validate_pattern a) (fun () -> validate_pattern b)

and validate_expr =
  let open Bcfg_query in
  function
  | EWord _ -> Ok ()
  | EPattern p -> validate_pattern p
  | EGet_parameter (e, idx) ->
      begin match int_of_string_opt idx with
      | Some n when n >= 0 -> validate_expr e
      | _ -> Error (`Msg (Printf.sprintf "Invalid index %S in the query" idx))
      end
  | EGet_subdirective (a, b) ->
      Result.bind (validate_expr a) (fun () -> validate_expr b)
  | EDirective (e, p) ->
      Result.bind (validate_expr e) (fun () -> validate_pattern p)
  | EParameter (p, e) | EChild (p, e) | ENot_parameter (p, e) | ENot_child (p, e)
    ->
      Result.bind (validate_pattern p) (fun () -> validate_expr e)

let of_string str =
  let lexbuf = Lexing.from_string str in
  match Bcfg_query_parser.query Bcfg_query_lexer.token lexbuf with
  | query -> Result.bind (validate_expr query) (fun () -> Ok query)
  | exception Bcfg_query_parser.Error -> error_msgf "Invalid query: %S" str
  | exception Bcfg_query_lexer.Unexpected_character chr ->
      error_msgf "Invalid character %C in the query" chr
  | exception Bcfg_query_lexer.Unterminated_quote ->
      error_msgf "Unterminated quote in the query: %S" str

let pp = Bcfg_query.pp_expr

(* A query is [is_streamable] when it contains no [@(...)] substitution: such a
   query never looks outside the current directive's subtree, so it can be
   evaluated one top-level directive at a time (see {!Bcfg.Stream.to_directives})
   without materialising the whole document. *)
let rec eval_in_pattern =
  let open Bcfg_query in
  function
  | PWord _ | PAny -> false
  | PEval _ -> true
  | PNot p -> eval_in_pattern p
  | PAnd (a, b) | POr (a, b) -> eval_in_pattern a || eval_in_pattern b

and eval_in_expr =
  let open Bcfg_query in
  function
  | EWord _ -> false
  | EPattern p -> eval_in_pattern p
  | EGet_parameter (e, _) -> eval_in_expr e
  | EGet_subdirective (a, b) -> eval_in_expr a || eval_in_expr b
  | EDirective (e, p) -> eval_in_expr e || eval_in_pattern p
  | EParameter (p, e) | EChild (p, e) | ENot_parameter (p, e) | ENot_child (p, e)
    ->
      eval_in_pattern p || eval_in_expr e

let is_streamable query = not (eval_in_expr query)

(* The string value of a directive, as used inside a [$(...)] substitution. As
   noted in the design, [$(foo.bar)] behaves like [foo.bar[0]]: the value is the
   first parameter, and falls back to the directive name when there is none
   (which is precisely what [\[0\]] produces). *)
let value_of_directive { Bcfg.name; parameters; _ } =
  match parameters with p :: _ -> p | [] -> name

(* [predicate] and [eval] are mutually recursive: a [$(...)] pattern evaluates an
   expression against [root] (the whole document, so it can reference any part of
   it) and matches a string against the resulting values. *)
let rec predicate ~root pattern str =
  let open Bcfg_query in
  match pattern with
  | PWord word -> String.equal word str
  | PAny -> true
  | PEval expr ->
      let ds = eval ~root expr root in
      List.exists (fun d -> String.equal (value_of_directive d) str) ds
  | PNot p -> not (predicate ~root p str)
  | PAnd (a, b) -> predicate ~root a str && predicate ~root b str
  | POr (a, b) -> predicate ~root a str || predicate ~root b str

and eval ~root query bcfg =
  let open Bcfg_query in
  match query with
  | EWord word ->
      let fn { Bcfg.name; _ } = name = word in
      List.filter fn bcfg
  | EGet_subdirective (a, b) ->
      let bcfg = eval ~root a bcfg in
      let fn { Bcfg.children; _ } = eval ~root b children in
      List.concat_map fn bcfg
  | EGet_parameter (a, idx) -> begin
      let bcfg = eval ~root a bcfg in
      (* [idx] was checked by [validate_expr]: it is a valid number. *)
      match int_of_string_opt idx with
      | None -> []
      | Some idx ->
          let fn { Bcfg.parameters; children; _ } =
            match List.nth_opt parameters idx with
            | Some name -> Some { Bcfg.name; parameters = []; children }
            | None -> None
          in
          List.filter_map fn bcfg
    end
  | EDirective (a, p) ->
      let pred = predicate ~root p in
      let fn { Bcfg.name; _ } = pred name in
      let bcfg = eval ~root a bcfg in
      List.filter fn bcfg
  | EParameter (p, a) ->
      let pred = predicate ~root p in
      let fn { Bcfg.parameters; _ } = List.exists pred parameters in
      let bcfg = List.filter fn bcfg in
      eval ~root a bcfg
  | EChild (p, a) ->
      (* keep directives that contain a child whose name matches [p] *)
      let pred = predicate ~root p in
      let fn { Bcfg.children; _ } =
        List.exists (fun c -> pred c.Bcfg.name) children
      in
      let bcfg = List.filter fn bcfg in
      eval ~root a bcfg
  | ENot_parameter (p, a) ->
      (* anti-join: keep directives with NO parameter matching [p] *)
      let pred = predicate ~root p in
      let fn { Bcfg.parameters; _ } = not (List.exists pred parameters) in
      let bcfg = List.filter fn bcfg in
      eval ~root a bcfg
  | ENot_child (p, a) ->
      (* anti-join: keep directives with NO child matching [p] *)
      let pred = predicate ~root p in
      let fn { Bcfg.children; _ } =
        not (List.exists (fun c -> pred c.Bcfg.name) children)
      in
      let bcfg = List.filter fn bcfg in
      eval ~root a bcfg
  | EPattern p ->
      let pred = predicate ~root p in
      let fn { Bcfg.name; _ } = pred name in
      List.filter fn bcfg

let eval query bcfg = eval ~root:bcfg query bcfg
