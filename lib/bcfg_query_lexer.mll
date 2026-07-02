{
exception Unexpected_character of char
exception Unterminated_quote

open Bcfg_query_parser

let from_hex = function
  | '0' .. '9' as chr -> Char.code chr - Char.code '0'
  | 'a' .. 'f' as chr -> Char.code chr - Char.code 'a' + 10
  | 'A' .. 'F' as chr -> Char.code chr - Char.code 'A' + 10
  | _ -> assert false
    (* NOTE(dinosaure): our lexer can not recognize something else than these
       characters. It's safe to [assert false]. *)

let from_hex chr0 chr1 =
  let i0 = from_hex chr0
  and i1 = from_hex chr1 in
  Char.unsafe_chr ((i0 * 16) + i1)
}

let wsp = [ ' ' '\t' ]
let utf_8_tail = [ '\x80' - '\xbf' ]
let escape = '\\' [ '\\' '\'' ' ' '"' 'a' 'b' 't' 'n' 'v' 'f' 'r' '#' '{' '}' ]

let pchar = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '{' '}' '~' ]
let qchar = [ '\x21' - '\x26' '\x28' - '\x5b' '\x5d' - '\x7e' ]
let dqchar = [ '\x21' '\x23' - '\x5b' '\x5d' - '\x7e' ]

let hex = [ '0' - '9' 'a' - 'f' 'A' - 'F' ]

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
let utf_8_qchar = qchar | utf_8_rem
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
  | '*' { STAR }
  | ':' { COLON }
  | '^' { CARET }
  (* Substitution prefix: ['@'] is the recommended form (safe inside shell
     double quotes, unlike ['$'] which triggers command substitution); ['$'] is
     kept as an alias for jq familiarity. *)
  | '@' { DOLLAR }
  | '$' { DOLLAR }
  | wsp { token lexbuf }
  | eof { EOF }
  (* Digits are ordinary word characters: an index such as [foo[0]] is lexed
     as a [WORD] and converted into a number by the evaluator. *)
  | (utf_8_pchar+|escape)+ as word { WORD (Bcfg.unescape word) }
  (* Quoted words follow the same lexical rules as the configuration format
     itself. They allow a query to mention values that contain characters
     which are meaningful to the query language (['.'], digits, ['/'], ...),
     e.g. [server('www.example.org')]. *)
  | '"' { dquote (Buffer.create 0x10) lexbuf }
  | '\'' { quote (Buffer.create 0x10) lexbuf }
  | _ as chr { raise (Unexpected_character chr) }
and dquote buf = parse
  | '"' { WORD (Buffer.contents buf) }
  | "\\x" (hex as a) (hex as b)
    { Buffer.add_char buf (from_hex a b); dquote buf lexbuf }
  | (utf_8_dqchar+|escape)+ as str
    { Buffer.add_string buf (Bcfg.unescape str); dquote buf lexbuf }
  | wsp+ as str { Buffer.add_string buf str; dquote buf lexbuf }
  | eof { raise Unterminated_quote }
  | _ as chr { raise (Unexpected_character chr) }
and quote buf = parse
  | '\'' { WORD (Buffer.contents buf) }
  | "\\x" (hex as a) (hex as b)
    { Buffer.add_char buf (from_hex a b); quote buf lexbuf }
  | (utf_8_qchar+|escape)+ as str
    { Buffer.add_string buf (Bcfg.unescape str); quote buf lexbuf }
  | wsp+ as str { Buffer.add_string buf str; quote buf lexbuf }
  | eof { raise Unterminated_quote }
  | _ as chr { raise (Unexpected_character chr) }
