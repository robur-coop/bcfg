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

let () =
  Test.run
    [
      test_word;
      test_subdirective;
      test_index;
      test_parameter_pattern;
      test_peval;
      test_peval_index_equivalence;
    ]
