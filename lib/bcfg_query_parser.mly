%{
open Bcfg_query
%}

%token<string> WORD
%token<int> NUMBER
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token OR AND NOT
%token COMMA
%token DOT
%token DOLLAR
%token STAR
%token COLON
%token CARET
%token EOF

%left OR AND
%nonassoc PNOT
%nonassoc LBRACE
%nonassoc LLBRACE
%start <Bcfg_query.expr> query

%%

let pattern :=
  | ~ = WORD; <PWord>
  | STAR; { PAny }
  | DOLLAR; LBRACE; ~ = texpr; RBRACE; <PEval>
  | LBRACE; ~ = tpattern; RBRACE; <>
  | a = pattern; OR; b = pattern; { POr (a, b) }
  | a = pattern; AND; b = pattern; { PAnd (a, b) }
  | NOT; ~ = pattern; %prec PNOT <PNot>

let tpattern :=
  | OR; COMMA; lst = separated_nonempty_list(COMMA, pattern);
    { List.fold_left (fun acc x -> POr (acc, x)) (List.hd lst) (List.tl lst) }
  | AND; COMMA; lst = separated_nonempty_list(COMMA, pattern);
    { List.fold_left (fun acc x -> PAnd (acc, x)) (List.hd lst) (List.tl lst) }
  | ~ = pattern; <>

let expr :=
  | ~ = WORD; <EWord>
  | STAR; { EPattern PAny }
  | LBRACE; ~ = tpattern; RBRACE; %prec LLBRACE <EPattern>

let aexpr :=
  | ~ = expr; <>
  | e = aexpr; LBRACE; p = tpattern; RBRACE; { EParameter (p, e) }
  | e = aexpr; LBRACE; COLON; p = tpattern; RBRACE; { EChild (p, e) }
  | e = aexpr; LBRACE; CARET; p = tpattern; RBRACE; { ENot_parameter (p, e) }
  | e = aexpr; LBRACE; COLON; CARET; p = tpattern; RBRACE; { ENot_child (p, e) }
  | LBRACE; p = tpattern; RBRACE; e = aexpr; %prec LBRACE { EDirective (e, p) }

let texpr :=
  | ~ = aexpr; <>
  | a = texpr; DOT; b = aexpr; { EGet_subdirective (a, b) }
  | e = texpr; LBRACK; idx = NUMBER; RBRACK; { EGet_parameter (e, idx) }

let query := ~ = texpr; EOF; <>
