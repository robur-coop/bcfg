(* About [Fmt]:
   Copyright (c) 2016 The fmt programmers
   SPDX-License-Identifier: ISC *)

module Fmt = struct
  let string = Format.pp_print_string

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

type pattern =
  | PWord of string (* foo *)
  | PAny (* * *)
  | PEval of expr (* @(foo) *)
  | PNot of pattern (* !foo *)
  | PAnd of pattern * pattern (* foo&bar *)
  | POr of pattern * pattern (* foo|bar *)

and expr =
  | EGet_parameter of expr * int (* foo[0] *)
  | EGet_subdirective of expr * expr (* foo.bar *)
  | EDirective of expr * pattern (* (bar)foo *)
  | EParameter of pattern * expr (* foo(bar) *)
  | EChild of pattern * expr (* foo(:bar) *)
  | ENot_parameter of pattern * expr (* foo(^bar): no parameter matches bar *)
  | ENot_child of pattern * expr (* foo(:^bar): no child matches bar *)
  | EWord of string (* foo *)
  | EPattern of pattern (* (foo) *)

let rec pp_pattern ppf = function
  | PWord s -> Fmt.string ppf s
  | PAny -> Fmt.string ppf "*"
  | PEval e -> Fmt.pf ppf "@(%a)" pp_expr e
  | PNot p -> Fmt.pf ppf "!%a" pp_pattern p
  | PAnd (a, b) -> Fmt.pf ppf "%a&%a" pp_pattern a pp_pattern b
  | POr (a, b) -> Fmt.pf ppf "%a|%a" pp_pattern a pp_pattern b

and pp_expr ppf = function
  | EGet_parameter (e, idx) -> Fmt.pf ppf "%a[%d]" pp_expr e idx
  | EGet_subdirective (e0, e1) -> Fmt.pf ppf "%a.%a" pp_expr e0 pp_expr e1
  | EDirective (e, p) -> Fmt.pf ppf "(%a)%a" pp_expr e pp_pattern p
  | EParameter (p, e) -> Fmt.pf ppf "%a(%a)" pp_expr e pp_pattern p
  | EChild (p, e) -> Fmt.pf ppf "%a(:%a)" pp_expr e pp_pattern p
  | ENot_parameter (p, e) -> Fmt.pf ppf "%a(^%a)" pp_expr e pp_pattern p
  | ENot_child (p, e) -> Fmt.pf ppf "%a(:^%a)" pp_expr e pp_pattern p
  | EWord s -> Fmt.pf ppf "%S" s
  | EPattern p -> Fmt.pf ppf "(%a)" pp_pattern p
