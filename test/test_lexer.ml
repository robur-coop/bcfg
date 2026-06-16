let failf fmt = Format.kasprintf failwith fmt

let d ?(parameters = []) ?(children = []) name =
  { Bcfg.name; parameters; children }

let parse s =
  match Bcfg.parser (Lexing.from_string s) with
  | Ok t -> t
  | Error _ -> failf "cannot parse %S" s

let parse_err s =
  match Bcfg.parser (Lexing.from_string s) with
  | Ok _ -> failf "%S was expected to be rejected" s
  | Error _ -> ()

let one_param s =
  match parse s with
  | [ { Bcfg.parameters = [ p ]; _ } ] -> p
  | _ -> failf "%S did not parse to a single one-parameter directive" s

let eq_str ~msg a b =
  Test.check
    ~msg:(Format.sprintf "%s: expected %S, got %S" msg a b)
    (String.equal a b)

let test_named_escapes =
  Test.test ~title:"named escapes" @@ fun () ->
  eq_str ~msg:"\\n" "a\nb" (one_param "foo \"a\\nb\"\n");
  eq_str ~msg:"\\t" "a\tb" (one_param "foo \"a\\tb\"\n");
  eq_str ~msg:"\\\\" "a\\b" (one_param "foo \"a\\\\b\"\n");
  eq_str ~msg:"\\\"" "a\"b" (one_param "foo \"a\\\"b\"\n")

let test_hex_escape =
  Test.test ~title:"hex escape" @@ fun () ->
  eq_str ~msg:"\\x41" "A" (one_param "foo \"\\x41\"\n");
  eq_str ~msg:"NUL kept" "a\x00b" (one_param "foo \"a\\x00b\"\n")

let test_rfc822_continuation =
  Test.test ~title:"rfc822 continuation" @@ fun () ->
  let s = "bar \"fooba\\\n     rfoob\\\n     arfoo\"\n" in
  eq_str ~msg:"joined" "foobarfoobarfoo" (one_param s)

let render ~margin t =
  Bcfg.emitter ~cfg:(Bcfg.Out.config ~margin ()) t
  |> List.of_seq |> String.concat ""

let test_wrap_roundtrip =
  Test.test ~title:"wrap round-trip" @@ fun () ->
  let value =
    "alpha beta gamma  delta epsilon zeta eta theta iota kappa lambda mu"
  in
  let original = parse (Printf.sprintf "foo %S\n" value) in
  for margin = 6 to 40 do
    let text = render ~margin original in
    Test.check
      ~msg:(Printf.sprintf "round-trip at margin %d" margin)
      (original = parse text)
  done

let test_unquoted_word =
  Test.test ~title:"unquoted word" @@ fun () ->
  Test.check ~msg:"url as a plain word"
    ([ d "website" ~parameters:[ "https://din.osau.re/" ] ]
    = parse "website https://din.osau.re/\n")

let test_utf8_ok =
  Test.test ~title:"utf-8 ok" @@ fun () ->
  Test.check ~msg:"utf-8 name" ([ d "h\xc3\xa9llo" ] = parse "h\xc3\xa9llo\n")

let test_utf8_invalid =
  Test.test ~title:"utf-8 invalid" @@ fun () ->
  (* a lone continuation byte is not valid UTF-8. *)
  parse_err "foo \xff\n";
  Test.check ~msg:"lone continuation byte rejected" true

let test_comments =
  Test.test ~title:"comments" @@ fun () ->
  Test.check ~msg:"full-line and trailing comments"
    ([ d "foo" ~parameters:[ "bar" ] ]
    = parse "# a comment\nfoo bar # trailing\n");
  Test.check ~msg:"leading '#' is a comment" ([] = parse "#foo\n")

let test_unescape =
  Test.test ~title:"unescape" @@ fun () ->
  eq_str ~msg:"named only" "a\nb\\c" (Bcfg.unescape "a\\nb\\\\c")

let () =
  Test.run
    [
      test_named_escapes;
      test_hex_escape;
      test_wrap_roundtrip;
      test_rfc822_continuation;
      test_unquoted_word;
      test_utf8_ok;
      test_utf8_invalid;
      test_comments;
      test_unescape;
    ]
