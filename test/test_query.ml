let failf fmt = Format.kasprintf failwith fmt

let parse s =
  match Bcfg.parser (Lexing.from_string s) with
  | Ok t -> t
  | Error _ -> failf "cannot parse %S" s

let q s =
  match Bcfgq.of_string s with
  | Ok e -> e
  | Error (`Msg m) -> failf "cannot compile query %S: %s" s m

let cfg =
  parse
    "me {\n\
    \  username dinosaure\n\
    \  website https://din.osau.re/\n\
    \  uid 1000\n\
     }\n\
     account dinosaure {\n\
    \  service github\n\
     }\n\
     account hannes {\n\
    \  service gitlab\n\
     }\n"

let run query = Bcfgq.eval (q query) cfg
let names_of ds = List.map (fun d -> d.Bcfg.name) ds

(* the single parameter of a single-directive result *)
let one_param query =
  match run query with
  | [ { Bcfg.parameters = [ p ]; _ } ] -> p
  | ds ->
      failf "%S did not return a single one-parameter directive (got %d)" query
        (List.length ds)

let test_word =
  Test.test ~title:"word" @@ fun () ->
  Test.check ~msg:"two accounts" (List.length (run "account") = 2)

let test_subdirective =
  Test.test ~title:"subdirective" @@ fun () ->
  Test.check ~msg:"me.username"
    (String.equal "dinosaure" (one_param "me.username"))

let test_index =
  Test.test ~title:"index" @@ fun () ->
  Test.check ~msg:"account[0]"
    ([ "dinosaure"; "hannes" ] = names_of (run "account[0]"))

let test_parameter_pattern =
  Test.test ~title:"parameter pattern" @@ fun () ->
  Test.check ~msg:"account(hannes)" (List.length (run "account(hannes)") = 1);
  Test.check ~msg:"account(dinosaure|hannes)"
    (List.length (run "account(dinosaure|hannes)") = 2)

let test_peval =
  Test.test ~title:"peval" @@ fun () ->
  Test.check ~msg:"account($(me.username)).service"
    (String.equal "github" (one_param "account($(me.username)).service"))

let test_peval_index_equivalence =
  Test.test ~title:"peval/index equivalence" @@ fun () ->
  Test.check ~msg:"equivalence"
    (List.length (run "account($(me.username))")
    = List.length (run "account($(me.username[0]))"))

let test_wildcard =
  Test.test ~title:"wildcard" @@ fun () ->
  Test.check ~msg:"top-level *" (List.length (run "*") = 3);
  Test.check ~msg:"account.* names"
    ([ "service"; "service" ] = names_of (run "account.*"));
  Test.check ~msg:"account(*)" (List.length (run "account(*)") = 2)

let test_child =
  Test.test ~title:"child filter (:)" @@ fun () ->
  Test.check ~msg:"*(:service)"
    ([ "account"; "account" ] = names_of (run "*(:service)"));
  Test.check ~msg:"*(:username)" ([ "me" ] = names_of (run "*(:username)"));
  Test.check ~msg:"account(:service)" (List.length (run "account(:service)") = 2);
  Test.check ~msg:"none" (run "*(:nope)" = [])

let test_antijoin =
  Test.test ~title:"anti-join (^ and :^)" @@ fun () ->
  Test.check ~msg:"*(:^service)" ([ "me" ] = names_of (run "*(:^service)"));
  Test.check ~msg:"account(:^service)" (run "account(:^service)" = []);
  Test.check ~msg:"account(^dinosaure)"
    ([ "hannes" ] = names_of (run "account(^dinosaure)[0]"))

let test_quoting =
  Test.test ~title:"quoted words" @@ fun () ->
  Test.check ~msg:"single quotes"
    (String.equal "https://din.osau.re/"
       (one_param "me.website('https://din.osau.re/')"));
  Test.check ~msg:"double quotes" (List.length (run "account(\"hannes\")") = 1);
  Test.check ~msg:"quoted word as directive name"
    (String.equal "dinosaure" (one_param "'me'.username"));
  Test.check ~msg:"unterminated quote"
    (Result.is_error (Bcfgq.of_string "account('oops"))

let test_number_pattern =
  Test.test ~title:"number as pattern" @@ fun () ->
  Test.check ~msg:"me.uid(1000)"
    (String.equal "1000" (one_param "me.uid(1000)"));
  Test.check ~msg:"overflow"
    (Result.is_error (Bcfgq.of_string "foo[9999999999999999999999]"));
  Test.check ~msg:"non-numeric index"
    (Result.is_error (Bcfgq.of_string "foo[bar]"))

let test_pp_iso =
  Test.test ~title:"pp isomorphism" @@ fun () ->
  let iso s =
    let e = q s in
    let s' = Format.asprintf "%a" Bcfgq.pp e in
    e = q s'
  in
  Test.check ~msg:"bare" (iso "account(dinosaure|hannes).service[0]");
  Test.check ~msg:"quoted" (iso "me.website('https://din.osau.re/')");
  Test.check ~msg:"child/anti-join" (iso "*(:^service)(^dinosaure)");
  Test.check ~msg:"peval" (iso "account(@(me.username)).service");
  Test.check ~msg:"directive pattern" (iso "(dinosaure|hannes).service");
  Test.check ~msg:"nested operators" (iso "account((dinosaure|hannes)&!me)");
  Test.check ~msg:"prefixed operators" (iso "account((|,dinosaure,hannes))")

let () =
  Test.run
    [
      test_word;
      test_subdirective;
      test_index;
      test_parameter_pattern;
      test_peval;
      test_peval_index_equivalence;
      test_wildcard;
      test_child;
      test_antijoin;
      test_quoting;
      test_number_pattern;
      test_pp_iso;
    ]
