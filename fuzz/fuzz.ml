open Crowbar

let uchar =
  map [ int32 ] @@ fun n ->
  let n = Int32.to_int n land 0xfffffff mod 0x10ffff in
  try Uchar.of_int n with _exn -> bad_test ()

let word =
  map [ list1 uchar ] @@ fun vs ->
  let buf = Buffer.create 0x7ff in
  List.iter (Uutf.Buffer.add_utf_8 buf) vs;
  Buffer.contents buf

let parameters = list word

let directive =
  fix @@ fun m ->
  map [ word; parameters; list m ] @@ fun name parameters children ->
  { Bcfg.name; parameters; children }

let function_from_seq seq =
  let cell = ref (None, seq) in
  let consume_remaining buf max = function
    | Some (str, off) ->
        let len = Int.min max (String.length str - off) in
        Bytes.blit_string str off buf 0 len;
        if len = String.length str - off then Some (None, len)
        else Some (Some (str, off + len), len)
    | None -> None
  in
  let rec consume buf max =
    let remaining, seq = !cell in
    match consume_remaining buf max remaining with
    | Some (remaining, len) ->
        cell := (remaining, seq);
        len
    | None ->
        begin match Seq.uncons seq with
        | None -> 0
        | Some (str, seq) ->
            cell := (Some (str, 0), seq);
            consume buf max
        end
  in
  consume

let () =
  add_test ~name:"isomorphism" [ list directive ] @@ fun t ->
  let seq = Bcfg.emitter t in
  let fn = function_from_seq seq in
  let lexbuf = Lexing.from_function fn in
  match Bcfg.parser lexbuf with
  | Ok t' -> check_eq ~pp:Bcfg.pp_as_ocaml_value t t'
  | Error _ ->
      let seq = Bcfg.emitter t in
      let sstr = List.of_seq seq in
      let str = String.concat "" sstr in
      failf
        "Impossible to parse the serialized form of: @[<hov>%a@]@\n\
         The serialized form: @[<hov>%a@]"
        Bcfg.pp_as_ocaml_value t
        (Hxd_string.pp Hxd.default)
        str

(* Isomorphism must hold even when values are wrapped at a (small) margin. *)
let () =
  add_test ~name:"isomorphism with margin" [ list directive; int ] @@ fun t n ->
  let margin = 4 + (abs n mod 116) in
  let cfg = Bcfg.Out.config ~margin () in
  let str = Bcfg.emitter ~cfg t |> List.of_seq |> String.concat "" in
  match Bcfg.parser (Lexing.from_string str) with
  | Ok t' -> check_eq ~pp:Bcfg.pp_as_ocaml_value t t'
  | Error _ ->
      failf "Cannot parse the wrapped form (margin %d): @[<hov>%a@]" margin
        (Hxd_string.pp Hxd.default)
        str

let () =
  add_test ~name:"stream of_t/to_t" [ list directive ] @@ fun t ->
  match Bcfg.Stream.to_t (Bcfg.Stream.of_t t) with
  | Ok t' -> check_eq ~pp:Bcfg.pp_as_ocaml_value t t'
  | Error _ -> failf "Bcfg.Stream.to_t failed"

let () =
  add_test ~name:"stream isomorphism" [ list directive ] @@ fun t ->
  let str =
    Bcfg.Stream.to_string (Bcfg.Stream.of_t t)
    |> List.of_seq |> String.concat ""
  in
  match Bcfg.parser (Lexing.from_string str) with
  | Ok t' -> check_eq ~pp:Bcfg.pp_as_ocaml_value t t'
  | Error _ ->
      failf "Cannot parse the streamed form: @[<hov>%a@]"
        (Hxd_string.pp Hxd.default)
        str
