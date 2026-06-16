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
  val write_indent : string -> width:int -> ctx -> t
  val writev : string list -> ctx -> t
  val of_seq : string Seq.t -> ctx -> t
  val to_seq : t -> string Seq.t
end = struct
  type t =
    | Write of { str : string; off : int; len : int; k : int -> t }
    | Done

  type ctx = {
    buffer : bytes;
    mutable pos : int;
    mutable line : int;
    mutable column : int;
  }

  let ctx () = { buffer = Bytes.create 0x10000; pos = 0; line = 1; column = 0 }
  let column { column; _ } = column
  let line { line; _ } = line

  let to_seq t =
    let rec go = function
      | Done -> Seq.Nil
      | Write { str; off; len; k } ->
          let sub = String.sub str off len in
          Seq.Cons (sub, fun () -> go (k len))
    in
    fun () -> go t

  let flush k0 t =
    if t.pos > 0 then
      let rec k1 flushed =
        if flushed < t.pos then
          Write
            {
              str = Bytes.unsafe_to_string t.buffer;
              off = flushed;
              len = t.pos - flushed;
              k = (fun bytes -> k1 (flushed + bytes));
            }
        else (
          t.pos <- 0;
          k0 t)
      in
      k1 0
    else k0 t

  let write str t =
    let max = Bytes.length t.buffer in
    let rec go off_str rem_str t =
      let len = min (max - t.pos) rem_str in
      Bytes.blit_string str off_str t.buffer t.pos len;
      t.pos <- t.pos + len;
      if rem_str > len then flush (go (off_str + len) (rem_str - len)) t
      else Done
    in
    go 0 (String.length str) t

  let rec go ~fn k len =
    match k len with
    | Done -> fn ()
    | Write { str; off; len; k } ->
        let k = go ~fn k in
        Write { str; off; len; k }

  let bind x fn =
    match x with
    | Done -> fn ()
    | Write { str; off; len; k } ->
        let k = go ~fn k in
        Write { str; off; len; k }

  let flush t = flush (Fun.const Done) t
  let ( let* ) = bind

  let newline t =
    let* () = write "\n" t in
    t.line <- t.line + 1;
    t.column <- 0;
    Done

  let not_lf = function '\n' -> false | _ -> true

  let write str t =
    assert (String.for_all not_lf str);
    let* () = write str t in
    t.column <- t.column + String.length str;
    Done

  (* Write an indentation [str] (spaces and/or tabs) but account for its visual
     [width] in the column, so that a tab can be counted as several columns for
     the purpose of margin/wrapping. *)
  let write_indent str ~width t =
    let* () = write str t in
    t.column <- t.column - String.length str + width;
    Done

  let writev sstr t =
    let rec go = function
      | [] -> Done
      | x :: r ->
          assert (String.for_all not_lf x);
          let* () = write x t in
          t.column <- t.column + String.length x;
          go r
    in
    go sstr

  let of_seq seq t =
    let rec go seq =
      match Seq.uncons seq with
      | None -> Done
      | Some (str, seq) ->
          assert (String.for_all not_lf str);
          let* () = write str t in
          t.column <- t.column + String.length str;
          go seq
    in
    go seq
end

type cfg = {
  indent : int;
  tab : bool;
  margin : int option;
  escape : [ `All_with_hex | `Normal ];
  hex : [ `Lower | `Upper ];
}

let config ?margin ?(indent = 2) ?(tab = false) ?(escape = `Normal)
    ?(hex = `Lower) () =
  { margin; indent; tab; escape; hex }

(* The indentation string for nesting [level] and its visual width. When [tab]
   is set, one tab character is emitted per level and [indent] plays the role of
   the tab stop (the visual width of a tab); otherwise [indent] spaces are
   emitted per level. *)
let make_indent cfg level =
  let width = level * cfg.indent in
  if cfg.tab then (String.make level '\t', width)
  else (String.make width ' ', width)

let[@inline] not_in_x80_to_xbf v = Char.code v lsr 6 <> 0b10
let[@inline] not_in_xa0_to_xbf v = Char.code v lsr 5 <> 0b101
let[@inline] not_in_x80_to_x9f v = Char.code v lsr 5 <> 0b100
let[@inline] not_in_x90_to_xbf v = Char.code v < 0x90 || 0xbf < Char.code v
let[@inline] not_in_x80_to_x8f v = Char.code v lsr 4 <> 0x8

let is_valid_utf_8 str =
  let rec go len str i =
    if i > len then true
    else
      match str.[i] with
      | '\x00' .. '\x7f' -> go len str (i + 1)
      | '\xc2' .. '\xdf' ->
          if i + 1 > len || not_in_x80_to_xbf str.[i + 1] then false
          else go len str (i + 2)
      | '\xe0' ->
          if
            i + 2 > len
            || not_in_xa0_to_xbf str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
          then false
          else go len str (i + 3)
      | '\xe1' .. '\xec' | '\xee' .. '\xef' ->
          if
            i + 2 > len
            || not_in_x80_to_xbf str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
          then false
          else go len str (i + 3)
      | '\xed' ->
          if
            i + 2 > len
            || not_in_x80_to_x9f str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
          then false
          else go len str (i + 3)
      | '\xf0' ->
          if
            i + 3 > len
            || not_in_x90_to_xbf str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
            || not_in_x80_to_xbf str.[i + 3]
          then false
          else go len str (i + 4)
      | '\xf1' .. '\xf3' ->
          if
            i + 3 > len
            || not_in_x80_to_xbf str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
            || not_in_x80_to_xbf str.[i + 3]
          then false
          else go len str (i + 4)
      | '\xf4' ->
          if
            i + 3 > len
            || not_in_x80_to_x8f str.[i + 1]
            || not_in_x80_to_xbf str.[i + 2]
            || not_in_x80_to_xbf str.[i + 3]
          then false
          else go len str (i + 4)
      | _ -> false
  in
  go (String.length str - 1) str 0

[@@@ocamlformat "disable"]

let safe_for_word = function
  | (* ! *) '\x21'
  | (* $ .. & *) '\x24' .. '\x26'
  | (* ( .. [ *) '\x28' .. '\x5b'
  | (* ] .. z *) '\x5d' .. '\x7a'
  | (* | *) '\x7c'
  | (* ~ *) '\x7e' -> true
  (* can be escaped *)
  | '\x07' | '\x08' | '\x09' | '\x0a' | '\x0b' | '\x0c' | '\x0d'
  | (* double quote *) '\x22' 
  | (* \ *) '\x5c' -> true
  | _ -> false

let escape_with_slash = function
  | '\x07' -> "\\a"
  | '\x08' -> "\\b"
  | '\x09' -> "\\t"
  | '\x0a' -> "\\n"
  | '\x0b' -> "\\v"
  | '\x0c' -> "\\f"
  | '\x0d' -> "\\r"
  | '\x22' -> "\\\""
  | '\x5c' -> "\\\\"
  | _ -> assert false

let word str =
  assert (String.for_all safe_for_word str);
  let buf = Buffer.create (String.length str) in
  let fn = function
    | '\x07' | '\x08' | '\x09' | '\x0a' | '\x0b' | '\x0c' | '\x0d' | '\x22' | '\x5c' as chr ->
        Buffer.add_string buf (escape_with_slash chr)
    | chr -> Buffer.add_char buf chr
  in
  String.iter fn str;
  Buffer.contents buf

let is_valid_word str = is_valid_utf_8 str && String.for_all safe_for_word str

let iter_on_utf_8 fn str =
  let rec go idx =
    if idx < String.length str then
      let dec = String.get_utf_8_uchar str idx in
      if Uchar.utf_decode_is_valid dec then begin
        match Uchar.utf_decode_length dec with
        | 1 ->
            fn (`Char str.[idx]);
            go (idx + 1)
        | len ->
            let tmp = String.sub str idx len in
            fn (`Uchar tmp);
            go (idx + len)
      end
      else begin
        fn (`Char str.[idx]);
        go (idx + 1)
      end
  in
  go 0

let should_I_escape ~cfg = function
  | (*   *) '\x20' 
  | (* ! *) '\x21' 
  | (* # .. [ *) '\x23' .. '\x5b' 
  | (* ] .. ~ *) '\x5d' .. '\x7e' -> None
  | '\x07' | '\x08' | '\x09' | '\x0a' | '\x0b' | '\x0c' | '\x0d'
  | (* double quote *) '\x22'
  | (* \ *) '\x5c' ->
      if cfg.escape = `Normal
      then Some `With_slash
      else Some `With_hex
  | _ -> Some `With_hex

(* without love *)
[@@@ocamlformat "enable"]

let escape_with_hex ~cfg chr =
  match cfg.hex with
  | `Lower -> Format.sprintf "\\x%02x" (Char.code chr)
  | `Upper -> Format.sprintf "\\x%02X" (Char.code chr)

let escape_dqword ~cfg str =
  let fn = function
    | `Uchar str -> str
    | `Char chr -> (
        match should_I_escape ~cfg chr with
        | Some `With_slash -> escape_with_slash chr
        | Some `With_hex -> escape_with_hex ~cfg chr
        | None -> String.make 1 chr)
  in
  let acc = ref [] in
  let fn v = acc := fn v :: !acc in
  iter_on_utf_8 fn str;
  List.to_seq (List.rev !acc)

let double_quote = "\""

(* The RFC822-like line folding ([\ <newline> <whitespace>]) makes the lexer
   discard the indentation of a continuation line. A space of the {b value} that
   would land right after that indentation must therefore be escaped as
   ["\x20"], otherwise it would be swallowed on the next parse. (Other
   whitespace -- tabs, control characters -- has already been escaped by
   {!escape_dqword}, so the only literal blank reaching this function is the
   space; a space sitting just before the [\] is preserved by the lexer.) *)
let wrap ~indent ~margin seq t =
  let open Writer in
  let space = "\\x20" in
  let rec go line_start seq =
    match Seq.uncons seq with
    | None -> Writer.write double_quote t
    | Some (str, seq)
      when (not line_start) && Writer.column t + String.length str > margin ->
        let* () = Writer.write "\\" t in
        let* () = Writer.newline t in
        let* () = Writer.write (String.make indent ' ') t in
        let* () = Writer.write (if str = " " then space else str) t in
        go false seq
    | Some (str, seq) ->
        let* () = Writer.write str t in
        go false seq
  in
  let* () = Writer.write double_quote t in
  go true seq

let word_or_split ~cfg str t =
  if is_valid_word str then
    match cfg.margin with
    | None -> Some (word str)
    | Some margin ->
        let column = Writer.column t in
        let word = word str in
        if column + String.length word < margin then Some word else None
  else None

let string ~cfg str t =
  match word_or_split ~cfg str t with
  | Some word -> Writer.write word t
  | None -> (
      let open Writer in
      match cfg.margin with
      | None ->
          let* () = Writer.write double_quote t in
          let* () = Writer.of_seq (escape_dqword ~cfg str) t in
          let* () = Writer.write double_quote t in
          Writer.flush t
      | Some margin ->
          let seq = escape_dqword ~cfg str in
          let column = Writer.column t in
          let* () = wrap ~indent:(column + 1) ~margin seq t in
          Writer.flush t)

let parameters ~cfg parameters t =
  let open Writer in
  let rec go = function
    | [] -> Writer.flush t
    | p :: ps ->
        let* () = Writer.write " " t in
        let* () = string ~cfg p t in
        go ps
  in
  go parameters

open Bcfg_type

let rec directive ~cfg ~level { name; parameters = ps; children } t =
  let open Writer in
  let istr, iwidth = make_indent cfg level in
  let* () = Writer.write_indent istr ~width:iwidth t in
  let* () = string ~cfg name t in
  let* () = parameters ~cfg ps t in
  match children with
  | [] -> Writer.newline t
  | children ->
      let* () = Writer.write " {" t in
      let* () = Writer.newline t in
      let rec go = function
        | [] ->
            let* () = Writer.write_indent istr ~width:iwidth t in
            let* () = Writer.write "}" t in
            Writer.newline t
        | x :: r ->
            let* () = directive ~cfg ~level:(level + 1) x t in
            go r
      in
      go children

let emitter ~cfg directives t =
  let open Writer in
  let rec go = function
    | [] -> Writer.flush t
    | x :: r ->
        let* () = directive ~cfg ~level:0 x t in
        go r
  in
  go directives

(* Streaming emitter: consumes a stream of {!Bcfg_type.Stream.lexeme}s pulled from
   [next] and produces the very same output as {!emitter} would for the
   equivalent tree, without ever materialising it. The only state kept is the
   current nesting depth (for indentation) and, per currently-open directive,
   whether it has a children block (so that {!Bcfg_type.Stream.De} knows whether
   the closing newline has already been emitted by {!Bcfg_type.Stream.Oe}). *)
let lexeme_emitter ~cfg next t =
  let open Writer in
  let depth = ref 0 in
  let stack = ref [] in
  let rec go () =
    match next () with
    | None -> Writer.flush t
    | Some (Bcfg_type.Stream.Ds name) ->
        let istr, iwidth = make_indent cfg !depth in
        let* () = Writer.write_indent istr ~width:iwidth t in
        let* () = string ~cfg name t in
        stack := ref false :: !stack;
        go ()
    | Some (Bcfg_type.Stream.P p) ->
        let* () = Writer.write " " t in
        let* () = string ~cfg p t in
        go ()
    | Some Bcfg_type.Stream.Os ->
        (match !stack with r :: _ -> r := true | [] -> ());
        let* () = Writer.write " {" t in
        let* () = Writer.newline t in
        incr depth;
        go ()
    | Some Bcfg_type.Stream.Oe ->
        if !depth > 0 then decr depth;
        let istr, iwidth = make_indent cfg !depth in
        let* () = Writer.write_indent istr ~width:iwidth t in
        let* () = Writer.write "}" t in
        let* () = Writer.newline t in
        go ()
    | Some Bcfg_type.Stream.De -> (
        match !stack with
        | r :: rest ->
            stack := rest;
            if !r then go ()
            else
              let* () = Writer.newline t in
              go ()
        | [] ->
            let* () = Writer.newline t in
            go ())
  in
  go ()
