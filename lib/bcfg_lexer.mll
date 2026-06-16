{
exception Unexpected_character of char
exception Invalid_word of string

let escape_to_char = function
  | '\\' -> '\\'
  | '\'' -> '\x27'
  | ' ' -> '\x20'
  | '"' -> '\x22'
  | '#' -> '\x23'
  | 'a' -> '\x07'
  | 'b' -> '\x08'
  | 't' -> '\x09'
  | 'n' -> '\x0a'
  | 'v' -> '\x0b'
  | 'f' -> '\x0c'
  | 'r' -> '\x0d'
  | '{' -> '\x7b'
  | '}' -> '\x7d'
  | _ -> assert false
    (* NOTE(dinosaure): our lexer can not recognize something else than these
       characters. It's safe to [assert false]. *)

let unescape str =
  let buf = Buffer.create (String.length str) in
  let add_rem buf str anchor idx =
    let len = idx - anchor in
    Buffer.add_substring buf str anchor len in
  let rec go len anchor idx =
    if idx >= len then begin
      add_rem buf str anchor idx;
      Buffer.contents buf
    end else if str.[idx] = '\\' && idx+1 < len
    then begin
      add_rem buf str anchor idx;
      Buffer.add_char buf (escape_to_char str.[idx+1]);
      go len (idx+2) (idx+2)
    end else go len anchor (idx+1) in
  go (String.length str) 0 0

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

open Bcfg_parser

(* Some notes about our lexer:
   1) The format is what is known as 'line-directed'. This means that the
      character "\n" has a specific meaning in the syntax of our format. We
      therefore generate the [NEWLINE] token.

   [pchar] are all printable characters except:
   - '\x22' (double quote)
   - '\x27' (quote)
   - \
   - {
   - }
 *)
}

let wsp = [ ' ' '\t' ]
let comment = wsp* '#' [ ^ '\n' ]*
let newline = wsp* comment? '\n'
let utf_8_tail = [ '\x80' - '\xbf' ]
let escape = '\\' [ '\\' '\'' ' ' '"' 'a' 'b' 't' 'n' 'v' 'f' 'r' '#' '{' '}' ]

let pchar = [ '\x21' '\x23' - '\x26' '\x28' - '\x5b' '\x5d' - '\x7a' '\x7c' '\x7e' ]
let qchar = [ '\x21' - '\x26' '\x28' - '\x5b' '\x5d' - '\x7e' ]
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
let utf_8_qchar = qchar | utf_8_rem
let utf_8_dqchar = dqchar | utf_8_rem

rule token = parse
  | '{' { LBRACE }
  | '}' { RBRACE }
  | wsp { token lexbuf }
  | eof { EOF }
  | comment? '\n' { Lexing.new_line lexbuf; NEWLINE }
    (* NOTE(dinosaure): [comment] takes precedence over [WORD], meaning that you
       cannot start a directive with "#"; you must use single or double quotes
       to use such a character. *)
  | (utf_8_pchar+|escape)+ as word { WORD (unescape word) }
  | '"' { dquote [] lexbuf }
  | '\'' { quote [] lexbuf }
  | _ as chr { raise (Unexpected_character chr) }
and dquote acc = parse
  | '"' { let ws = List.rev_map unescape acc in
          WORD (String.concat "" ws) }

  (* RFC822-like value *)
  | ((utf_8_dqchar+|escape)+ as word) "\\" wsp* comment? "\n" wsp+
    { Lexing.new_line lexbuf; dquote (word :: acc) lexbuf }
  | "\\" wsp* comment? "\n" wsp+
    { Lexing.new_line lexbuf; dquote acc lexbuf }

  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    { dquote (String.make 1 (from_hex a b) :: acc) lexbuf }

  | (utf_8_dqchar+|escape)+ as word { dquote (word :: acc) lexbuf }
  | wsp+ as wsp { dquote (wsp :: acc) lexbuf }
  | _ as chr { raise (Unexpected_character chr) }
and quote acc = parse
  | '\'' { let ws = List.rev_map unescape acc in
           WORD (String.concat "" ws) }
  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    { quote (String.make 1 (from_hex a b) :: acc) lexbuf }
  | (utf_8_qchar+|escape)+ as word { quote (word :: acc) lexbuf }
  | wsp+ as wsp { quote (wsp :: acc) lexbuf }
  | _ as chr { raise (Unexpected_character chr) }
