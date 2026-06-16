let failf fmt = Format.kasprintf failwith fmt

let parse s =
  match Bcfg.parser (Lexing.from_string s) with
  | Ok t -> t
  | Error _ -> failf "cannot parse %S" s

let json_of s = Bcfg_json.to_string ~minify:true (Bcfg_json.of_config (parse s))

let case ~title expected s =
  Test.test ~title @@ fun () ->
  let got = json_of s in
  Test.check
    ~msg:(Format.sprintf "expected %s, got %s" expected got)
    (String.equal expected got)

let () =
  Test.run
    [
      case ~title:"scalar" {|{"website":"https://din.osau.re/"}|}
        "website https://din.osau.re/\n";
      case ~title:"no param" {|{"flag":null}|} "flag\n";
      case ~title:"many params" {|{"a":["b","c"]}|} "a b c\n";
      case ~title:"children" {|{"user":{"name":"dinosaure","age":"34"}}|}
        "user {\n  name dinosaure\n  age 34\n}\n";
      case ~title:"params and children"
        {|{"user":{"$params":"dinosaure","age":"34"}}|}
        "user dinosaure {\n  age 34\n}\n";
      case ~title:"duplicates" {|{"tag":["a","b"]}|} "tag a\ntag b\n";
      case ~title:"escaping" {|{"k":"a\"b\n"}|} "k \"a\\\"b\\n\"\n";
    ]
