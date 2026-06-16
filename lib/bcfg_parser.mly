%{

%}

%token<string> WORD
%token LBRACE
%token RBRACE
%token NEWLINE
%token EOF

%start <Bcfg_type.t> config

%%

let parameters ==
  | ~ = list(WORD); <>

let children :=
  | LBRACE; NEWLINE+; ~ = list(directive); RBRACE; NEWLINE+; <>
  | NEWLINE+; { [] }

let directive :=
  | name = WORD; ~ = parameters; ~ = children;
    { { name; parameters; children } }

let config :=
  | NEWLINE*; ~ = list(directive); EOF; <>
