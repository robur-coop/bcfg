{
exception Unexpected_character of char

open Bcfg_query_parser
}

let wsp = [ ' ' '\t' ]
let utf_8_tail = [ '\x80' - '\xbf' ]
let escape = '\\' [ '\\' '"' 'a' 'b' 't' 'n' 'v' 'f' 'r' '#' ]

let pchar = [ 'a' - 'z' 'A' - 'Z' '{' '}' '~' ]
let dqchar = [ '\x21' '\x23' - '\x5b' '\x5d' - '\x7e' ]

let utf_8_rem =
  ([ '\xc2' - '\xdf' ] utf_8_tail)
  | ('\xe0' [ '\xa0' - '\xbf' ] utf_8_tail)
  | ([ '\xe1' - '\xec' ] utf_8_tail utf_8_tail)
  | ('\xed' [ '\x80' - '\x9f' ] utf_8_tail)
  | ([ '\xee' - '\xef' ] utf_8_tail utf_8_tail)
  | ('\xf0' [ '\x90' - '\xbf' ] utf_8_tail utf_8_tail)
  | ([ '\xf1' - '\xf3' ] utf_8_tail utf_8_tail utf_8_tail)
  | ('\xf4' [ '\x80' - '\x8f' ] utf_8_tail utf_8_tail)

let utf_8_pchar = pchar | utf_8_rem
let utf_8_dqchar = dqchar | utf_8_rem

rule token = parse
  | '(' { LBRACE }
  | ')' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '!' { NOT }
  | '|' { OR }
  | '&' { AND }
  | ',' { COMMA }
  | '.' { DOT }
  | '$' { DOLLAR }
  | wsp { token lexbuf }
  | eof { EOF }
  | (utf_8_pchar+|escape)+ as word { WORD (Bcfg.unescape word) }
  | [ '0' - '9' ]+ as number { NUMBER (int_of_string number) (* TODO *) }
  | _ as chr { raise (Unexpected_character chr) }
