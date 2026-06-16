module Stream = Bcfg_type.Stream

type lexeme = Stream.lexeme = Ds of string | P of string | Os | Oe | De

let pp_lexeme = Stream.pp

type error =
  [ `Lexer_error of
    Bcfg_txtloc.t * [ `Invalid_character of char | `Message of string ]
  | `Parser_error of Bcfg_txtloc.t * string ]

let pp_error ppf = function
  | `Lexer_error (loc, `Invalid_character chr) ->
      Format.fprintf ppf "%a: invalid character %C" Bcfg_txtloc.pp loc chr
  | `Lexer_error (loc, `Message msg) ->
      Format.fprintf ppf "%a: %s" Bcfg_txtloc.pp loc msg
  | `Parser_error (loc, msg) ->
      Format.fprintf ppf "%a: %s" Bcfg_txtloc.pp loc msg

(* {1 Decoding.}

   A hand-written recursive-descent state machine over the token stream produced
   by {!Bcfg_lexer.token}. Each function returns a thunk producing the next
   [Seq.node], so tokens are pulled lazily, as the resulting sequence is forced.
   The only memory retained while decoding is the chain of continuations, i.e.
   the current nesting depth. *)

let to_seq lexbuf : (lexeme, error) result Seq.t =
  let read () =
    try Ok (Bcfg_lexer.token lexbuf)
    with Bcfg_lexer.Unexpected_character chr ->
      Error
        (`Lexer_error (Bcfg_txtloc.from_lexbuf lexbuf, `Invalid_character chr))
  in
  let perr msg = `Parser_error (Bcfg_txtloc.from_lexbuf lexbuf, msg) in
  let fail e () = Seq.Cons (Error e, fun () -> Seq.Nil) in
  let rec top () =
    match read () with
    | Error e -> fail e ()
    | Ok Bcfg_parser.NEWLINE -> top ()
    | Ok Bcfg_parser.EOF -> Seq.Nil
    | Ok (Bcfg_parser.WORD d) -> directive d top ()
    | Ok Bcfg_parser.LBRACE -> fail (perr "unexpected '{'") ()
    | Ok Bcfg_parser.RBRACE -> fail (perr "unexpected '}'") ()
  and directive d k () = Seq.Cons (Ok (Ds d), fun () -> params k ())
  and params k () =
    match read () with
    | Error e -> fail e ()
    | Ok (Bcfg_parser.WORD p) -> Seq.Cons (Ok (P p), fun () -> params k ())
    | Ok Bcfg_parser.LBRACE -> Seq.Cons (Ok Os, fun () -> children k ())
    | Ok Bcfg_parser.NEWLINE -> Seq.Cons (Ok De, k)
    | Ok Bcfg_parser.EOF -> Seq.Cons (Ok De, fun () -> Seq.Nil)
    | Ok Bcfg_parser.RBRACE -> fail (perr "unexpected '}' after a directive") ()
  and children k () =
    match read () with
    | Error e -> fail e ()
    | Ok Bcfg_parser.NEWLINE -> children k ()
    | Ok (Bcfg_parser.WORD d) -> directive d (fun () -> children k ()) ()
    | Ok Bcfg_parser.RBRACE ->
        (* '}' consumed: close the children block and end the owning directive.
           The trailing newline(s) after '}' are skipped by [k]. *)
        Seq.Cons (Ok Oe, fun () -> Seq.Cons (Ok De, k))
    | Ok Bcfg_parser.EOF -> fail (perr "unterminated children block '{'") ()
    | Ok Bcfg_parser.LBRACE -> fail (perr "unexpected '{'") ()
  in
  top

type decoder = { mutable seq : (lexeme, error) result Seq.t }

let decoder lexbuf = { seq = to_seq lexbuf }

let decode d =
  match d.seq () with
  | Seq.Nil -> Ok None
  | Seq.Cons (Ok lx, seq) ->
      d.seq <- seq;
      Ok (Some lx)
  | Seq.Cons (Error e, _) ->
      d.seq <- Seq.empty;
      Error e

(* {1 Encoding.} *)

let encode ?(cfg = Bcfg_out.config ()) next sink =
  let w = Bcfg_out.lexeme_emitter ~cfg next (Bcfg_out.Writer.ctx ()) in
  Seq.iter sink (Bcfg_out.Writer.to_seq w)

let to_string ?(cfg = Bcfg_out.config ()) seq =
  let s = ref seq in
  let next () =
    match !s () with
    | Seq.Nil -> None
    | Seq.Cons (x, r) ->
        s := r;
        Some x
  in
  let w = Bcfg_out.lexeme_emitter ~cfg next (Bcfg_out.Writer.ctx ()) in
  Bcfg_out.Writer.to_seq w

(* {1 Bridges with the tree representation.} *)

let of_t (t : Bcfg_type.t) : lexeme Seq.t =
  let rec dirs ds k () =
    match ds with [] -> k () | d :: ds -> dir d (fun () -> dirs ds k ()) ()
  and dir d k () =
    Seq.Cons (Ds d.Bcfg_type.name, fun () -> params d.parameters d.children k ())
  and params ps children k () =
    match ps with
    | p :: ps -> Seq.Cons (P p, fun () -> params ps children k ())
    | [] -> body children k ()
  and body children k () =
    match children with
    | [] -> Seq.Cons (De, k)
    | cs ->
        Seq.Cons
          ( Os,
            fun () ->
              dirs cs (fun () -> Seq.Cons (Oe, fun () -> Seq.Cons (De, k))) ()
          )
  in
  fun () -> dirs t (fun () -> Seq.Nil) ()

let nowhere = Bcfg_txtloc.Nowhere

let to_t seq =
  let result = ref [] in
  let stack = ref [] in
  let push_child d =
    match !stack with
    | (_, _, cs) :: _ -> cs := d :: !cs
    | [] -> result := d :: !result
  in
  let finish (name, ps, cs) =
    { Bcfg_type.name; parameters = List.rev !ps; children = List.rev !cs }
  in
  let rec go seq =
    match seq () with
    | Seq.Nil ->
        begin match !stack with
        | [] -> Ok (List.rev !result)
        | _ -> Error (`Parser_error (nowhere, "unterminated directive"))
        end
    | Seq.Cons (Ds name, seq) ->
        stack := (name, ref [], ref []) :: !stack;
        go seq
    | Seq.Cons (P p, seq) ->
        begin match !stack with
        | (_, ps, _) :: _ ->
            ps := p :: !ps;
            go seq
        | [] ->
            Error (`Parser_error (nowhere, "parameter outside of a directive"))
        end
    | Seq.Cons ((Os | Oe), seq) -> go seq
    | Seq.Cons (De, seq) ->
        begin match !stack with
        | top :: rest ->
            stack := rest;
            push_child (finish top);
            go seq
        | [] -> Error (`Parser_error (nowhere, "unbalanced end of directive"))
        end
  in
  go seq
