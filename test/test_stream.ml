let failf fmt = Format.kasprintf failwith fmt

let parse s =
  match Bcfg.parser (Lexing.from_string s) with
  | Ok t -> t
  | Error _ -> failf "cannot parse %S" s

let stream_of s =
  Bcfg.Stream.to_seq (Lexing.from_string s)
  |> Seq.map (function
    | Ok lx -> lx
    | Error e -> failf "stream error: %a" Bcfg.Stream.pp_error e)
  |> List.of_seq

let samples =
  [
    "foo bar\n";
    "foo\n";
    "a b c\n";
    "dinosaure {\n  website https://din.osau.re/\n}\n";
    "a {\n  b {\n    c d\n  }\n}\n";
    "dinosaure github {\n\
    \  website https://github.com/dinosaure\n\
     }\n\n\
     hannes github {\n\
    \  website https://github.com/hannesm\n\
     }\n";
  ]

let test_of_to =
  Test.test ~title:"of_t/to_t" @@ fun () ->
  List.iter
    (fun s ->
      let t = parse s in
      match Bcfg.Stream.to_t (Bcfg.Stream.of_t t) with
      | Ok t' -> Test.check ~msg:s (t = t')
      | Error e -> failf "to_t: %a" Bcfg.Stream.pp_error e)
    samples

let test_stream_matches_parser =
  Test.test ~title:"decoder matches parser" @@ fun () ->
  List.iter
    (fun s ->
      let expected = List.of_seq (Bcfg.Stream.of_t (parse s)) in
      Test.check ~msg:s (expected = stream_of s))
    samples

let test_iso_stream =
  Test.test ~title:"encode isomorphism" @@ fun () ->
  List.iter
    (fun s ->
      let t = parse s in
      let rendered =
        Bcfg.Stream.to_string (Bcfg.Stream.of_t t)
        |> List.of_seq |> String.concat ""
      in
      Test.check ~msg:s (t = parse rendered))
    samples

let test_deep =
  Test.test ~title:"deep nesting" @@ fun () ->
  let depth = 2000 in
  let buf = Buffer.create (depth * 8) in
  for _ = 1 to depth do
    Buffer.add_string buf "a {\n"
  done;
  Buffer.add_string buf "leaf v\n";
  for _ = 1 to depth do
    Buffer.add_string buf "}\n"
  done;
  let s = Buffer.contents buf in
  let n = List.length (stream_of s) in
  (* depth * (Ds a, Os) + (Ds leaf, P v, De) + depth * (Oe, De) *)
  Test.check ~msg:"lexeme count" ((depth * 4) + 3 = n);
  match Bcfg.Stream.to_t (List.to_seq (stream_of s)) with
  | Ok t -> Test.check ~msg:"deep round-trip" (parse s = t)
  | Error e -> failf "deep to_t: %a" Bcfg.Stream.pp_error e

let test_eof_leniency =
  Test.test ~title:"eof leniency" @@ fun () ->
  Test.check ~msg:"leaf at eof"
    ([ Bcfg.Stream.Ds "foo"; Bcfg.Stream.P "bar"; Bcfg.Stream.De ]
    = stream_of "foo bar")

let directives_of s =
  Bcfg.Stream.to_directives (Lexing.from_string s)
  |> Seq.map (function
    | Ok d -> d
    | Error e -> failf "to_directives: %a" Bcfg.Stream.pp_error e)
  |> List.of_seq

let test_to_directives =
  Test.test ~title:"to_directives" @@ fun () ->
  List.iter (fun s -> Test.check ~msg:s (parse s = directives_of s)) samples

let () =
  Test.run
    [
      test_of_to;
      test_stream_matches_parser;
      test_iso_stream;
      test_deep;
      test_eof_leniency;
      test_to_directives;
    ]
