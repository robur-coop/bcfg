let failf fmt = Format.kasprintf failwith fmt

let d ?(parameters = []) ?(children = []) name =
  { Bcfg.name; parameters; children }

let parse s = Bcfg.parser (Lexing.from_string s)
let ok s = match parse s with Ok t -> t | Error _ -> failf "cannot parse %S" s

let err s =
  match parse s with
  | Ok _ -> failf "%S was expected to be rejected" s
  | Error _ -> ()

let test_flat =
  Test.test ~title:"flat" @@ fun () ->
  Test.check ~msg:"two directives"
    ([ d "foo" ~parameters:[ "bar" ]; d "baz" ] = ok "foo bar\nbaz\n")

let test_parameters =
  Test.test ~title:"parameters" @@ fun () ->
  Test.check ~msg:"several parameters"
    ([ d "a" ~parameters:[ "b"; "c"; "d" ] ] = ok "a b c d\n")

let test_nesting =
  Test.test ~title:"nesting" @@ fun () ->
  Test.check ~msg:"nested children"
    ([
       d "a" ~parameters:[ "p" ]
         ~children:[ d "b" ~children:[ d "c" ~parameters:[ "d" ] ] ];
     ]
    = ok "a p {\n  b {\n    c d\n  }\n}\n")

let test_line_driven =
  Test.test ~title:"line-driven" @@ fun () ->
  err "foo { bar }\n";
  err "foo {\n  bar }\n";
  err "foo { bar\n}\n";
  Test.check ~msg:"one-liner blocks rejected" true

let test_leading_newlines =
  Test.test ~title:"leading newlines" @@ fun () ->
  Test.check ~msg:"leading blank lines are tolerated"
    ([ d "foo" ] = ok "\n\n\nfoo\n")

let test_unterminated =
  Test.test ~title:"unterminated block" @@ fun () ->
  err "foo {\n";
  Test.check ~msg:"unterminated block rejected" true

let test_stray_close =
  Test.test ~title:"stray close" @@ fun () ->
  err "}\n";
  Test.check ~msg:"stray close rejected" true

let test_empty =
  Test.test ~title:"empty" @@ fun () ->
  Test.check ~msg:"empty input" ([] = ok "")

let () =
  Test.run
    [
      test_flat;
      test_parameters;
      test_nesting;
      test_line_driven;
      test_leading_newlines;
      test_unterminated;
      test_stray_close;
      test_empty;
    ]
