type t =
  | Nowhere
  | Simple of { line : int; start : int; stop : int }
  | Multiline of { start : int * int; stop : int * int }

let pp ppf = function
  | Nowhere -> ()
  | Simple { line; start; stop } ->
      Format.fprintf ppf "l%d.%d-%d" line start stop
  | Multiline { start = l1, c1; stop = l2, c2 } ->
      Format.fprintf ppf "l%d.%d-l%d.%d" l1 c1 l2 c2

type 'a with_txtloc = { txtloc : t; contents : 'a }

let make txtloc contents = { txtloc; contents }
let nowhere contents = { txtloc = Nowhere; contents }

let expand = function
  | Nowhere -> None
  | Simple { line; start; stop } -> Some ((line, start), (line, stop))
  | Multiline m -> Some (m.start, m.stop)

let compress = function
  | Multiline { start; stop } when fst start = fst stop ->
      Simple { line = fst start; start = snd start; stop = snd stop }
  | (Simple _ | Multiline _ | Nowhere) as m -> m

let merge x y =
  compress
    begin match (expand x, expand y) with
    | None, None -> Nowhere
    | Some (start, stop), None
    | None, Some (start, stop)
    | Some (start, _), Some (_, stop) ->
        Multiline { start; stop }
    end

let from_lexbuf lexbuf =
  compress
    begin
      let p1 = Lexing.lexeme_start_p lexbuf in
      let p2 = Lexing.lexeme_end_p lexbuf in
      let start = (p1.pos_lnum, p1.pos_cnum - p1.pos_bol) in
      let stop = (p2.pos_lnum, p2.pos_cnum - p2.pos_bol) in
      Multiline { start; stop }
    end

let line_start = function
  | Nowhere -> invalid_arg "Txtloc.line_start"
  | Simple { line; _ } -> line
  | Multiline { start = line, _; _ } -> line

let line_end = function
  | Nowhere -> invalid_arg "Txtloc.line_end"
  | Simple { line; _ } -> line
  | Multiline { stop = line, _; _ } -> line

type line =
  | Errored_line of { err : string; around : string * string }
  | Context_line of string
  | Eof of { errored : bool }

let errored_line ~txtloc ~line_number line =
  assert (line_number >= line_start txtloc);
  assert (line_number <= line_end txtloc);
  match txtloc with
  | Simple { start; stop; _ } ->
      let line = line in
      let pre = String.sub line 0 start in
      let err = String.sub line start (stop - start) in
      let nxt = String.sub line stop (String.length line - stop) in
      Errored_line { err; around = (pre, nxt) }
  | Multiline { start = line_number', c; _ } when line_number = line_number' ->
      let line = line in
      let pre = String.sub line 0 c in
      let err = String.sub line c (String.length line - c) in
      Errored_line { err; around = (pre, "") }
  | Multiline { stop = line_number', c; _ } when line_number = line_number' ->
      let line = line in
      let err = String.sub line 0 c in
      let nxt = String.sub line c (String.length line - c) in
      Errored_line { err; around = ("", nxt) }
  | Multiline _ -> Errored_line { err = line; around = ("", "") }
  | Nowhere -> assert false

let context_line line = Context_line line

external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

(* NOTE(dinosaure): [Stdlib.input_line] can return a line which terminates with
   [EOF]. We cannot therefore assume that all lines returned by
   [Stdlib.input_line] end with [LF]. This reimplementation of [input_line]
   keeps the [LF] when it is present and allows us to know whether our last line
   ended with [EOF] or with [LF EOF].

   This is particularly important in cases of errors that can be raised by our
   lexer or parser, where every character (including [LF]) matters. *)
let input_line ic =
  let rec line buf pos = function
    | [] -> buf
    | hd :: tl ->
        let len = Bytes.length hd in
        Bytes.blit hd 0 buf (pos - len) len;
        line buf (pos - len) tl
  in
  let rec scan acc len =
    let n = input_scan_line ic in
    if n = 0 then
      match acc with
      | [] -> raise End_of_file
      | _ -> line (Bytes.create len) len acc
    else if n > 0 then (
      let res = Bytes.create n in
      ignore (input ic res 0 n);
      match acc with
      | [] -> res
      | _ ->
          let len = len + n in
          line (Bytes.create len) len (res :: acc))
    else
      let beg = Bytes.create (-n) in
      ignore (input ic beg 0 (-n));
      scan (beg :: acc) (len - n)
  in
  scan [] 0 |> Bytes.unsafe_to_string

(* Like {!input_line} but consuming a [string]: yields each line keeping its
   trailing ['\n'] when present, and raises [End_of_file] once exhausted. *)
let string_line_reader content =
  let len = String.length content in
  let pos = ref 0 in
  fun () ->
    if !pos >= len then raise End_of_file
    else
      match String.index_from_opt content !pos '\n' with
      | Some i ->
          let line = String.sub content !pos (i - !pos + 1) in
          pos := i + 1;
          line
      | None ->
          let line = String.sub content !pos (len - !pos) in
          pos := len;
          line

let lines_around ?(ctx = 1) ~txtloc read_line =
  let cstart = max 1 (line_start txtloc - ctx)
  and cend = line_end txtloc + ctx
  and lstart = max 1 (line_start txtloc)
  and lend = line_end txtloc in
  let rec go idx lines =
    match read_line () with
    | line ->
        (* errored lines *)
        if idx >= lstart && idx <= lend then
          go (succ idx)
            ((idx, errored_line ~txtloc ~line_number:idx line) :: lines)
          (* context lines *)
        else if idx >= cstart && idx <= cend then
          go (succ idx) ((idx, context_line line) :: lines)
        else if idx < cstart then go (succ idx) lines
        else List.rev lines
    | exception End_of_file ->
        if idx >= lstart && idx <= lend then
          List.rev ((idx, Eof { errored = true }) :: lines)
        else if idx >= lstart && idx <= lend then
          List.rev ((idx, Eof { errored = false }) :: lines)
        else List.rev lines
  in
  go 1 []

let lines_around_txtloc ?ctx ~txtloc ic =
  lines_around ?ctx ~txtloc (fun () -> input_line ic)

let lines_around_txtloc_string ?ctx ~txtloc content =
  lines_around ?ctx ~txtloc (string_line_reader content)
