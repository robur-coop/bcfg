open Bcfgt

let failwithf fmt = Format.kasprintf failwith fmt
let ok t = function Ok v -> v | Error (`Msg m) -> failwithf "%s: %s" t m

type server = {
  host : string;
  port : int;
  tls : bool;
  ratio : float;
  aliases : string list;
  admin : string option;
}

let t =
  let fn host port tls ratio aliases admin =
    { host; port; tls; ratio; aliases; admin }
  in
  directive ~name:"server" fn
  |> req ~pos:0 string (fun s -> s.host)
  |> field "port" int (fun s -> s.port)
  |> field "tls" bool (fun s -> s.tls)
  |> field "ratio" float (fun s -> s.ratio)
  |> field "aliases" (list string) (fun s -> s.aliases)
  |> opt "admin" string ~get:(fun s -> s.admin)
  |> uniq

exception Bcfg_error of Bcfg.error

let () =
  Printexc.register_printer @@ function
  | Bcfg_error err -> Some (Format.asprintf "%a" Bcfg.pp_error_for_human err)
  | _ -> None

let parse str =
  match Bcfg.parser (Lexing.from_string str) with
  | Ok t -> t
  | Error err -> raise (Bcfg_error err)

let render t = Bcfg.emitter t |> List.of_seq |> String.concat ""

let sample =
  {
    host = "example.com";
    port = 8080;
    tls = true;
    ratio = 0.5;
    aliases = [ "www"; "web" ];
    admin = Some "root";
  }

let test_encode_decode =
  Test.test ~title:"encode/decode" @@ fun () ->
  let v = ok "decode" (decode t (encode t sample)) in
  Test.check ~msg:"encode/decode" (sample = v)

let test_textual_roundtrip =
  Test.test ~title:"encode/decode (with text)" @@ fun () ->
  let text = render (encode t sample) in
  let v = ok "decode" (decode t (parse text)) in
  Test.check ~msg:"textual round-trip" (sample = v)

let test_optional_absent =
  Test.test ~title:"encode/decode (with absent)" @@ fun () ->
  let v =
    {
      host = "h";
      port = 1;
      tls = false;
      ratio = 1.;
      aliases = [];
      admin = None;
    }
  in
  let v' = ok "decode" (decode t (encode t v)) in
  Test.check ~msg:"absent option/empty list" (v = v')

let test_missing_field =
  Test.test ~title:"encode/decode (missing)" @@ fun () ->
  let cfg = parse "server h\n" in
  match decode t cfg with
  | Ok _ -> failwith "expected an error for the missing 'port' field"
  | Error (`Msg _) -> Test.check ~msg:"expected error" true

let test_bad_int =
  Test.test ~title:"bad int" @@ fun () ->
  let cfg = parse "server h {\n  port abc\n  tls true\n  ratio 1.\n}\n" in
  match decode t cfg with
  | Ok _ -> failwith "expected an error for the invalid integer"
  | Error (`Msg _) -> Test.check ~msg:"expected error" true

type tree = { label : string; kids : tree list }

let t =
  fix @@ fun tree ->
  uniq
    (directive ~name:"node" (fun label kids -> { label; kids })
    |> req ~pos:0 string (fun t -> t.label)
    |> field "kids" (list tree) (fun t -> t.kids))

let test_recursive =
  Test.test ~title:"recursive" @@ fun () ->
  let v =
    {
      label = "root";
      kids =
        [
          { label = "a"; kids = [ { label = "a1"; kids = [] } ] };
          { label = "b"; kids = [] };
        ];
    }
  in
  let v' = ok "decode" (decode t (encode t v)) in
  Test.check ~msg:"recursive round-trip" (v = v');
  let v'' = ok "decode" (decode t (parse (render (encode t v)))) in
  Test.check ~msg:"recursive textual round-trip" (v = v'')

type backend = Tcp of int | Unix of string

let backend =
  cases ~name:"backend" ~tag:"kind" string
    [
      case "tcp"
        (directive (fun port -> port) |> field "port" int Fun.id)
        ~inject:(fun p -> Tcp p)
        ~project:(function Tcp p -> Some p | _ -> None);
      case "unix"
        (directive (fun path -> path) |> field "path" string Fun.id)
        ~inject:(fun p -> Unix p)
        ~project:(function Unix p -> Some p | _ -> None);
    ]

let test_cases =
  Test.test ~title:"cases (sum type)" @@ fun () ->
  let roundtrip v =
    let v' = ok "decode" (decode backend (encode backend v)) in
    Test.check ~msg:"value round-trip" (v = v');
    let v'' =
      ok "decode" (decode backend (parse (render (encode backend v))))
    in
    Test.check ~msg:"textual round-trip" (v = v'')
  in
  roundtrip (Tcp 8080);
  roundtrip (Unix "/tmp/sock");
  let v =
    ok "decode" (decode backend (parse "backend {\n  kind tcp\n  port 80\n}\n"))
  in
  Test.check ~msg:"tag selects tcp" (v = Tcp 80);
  begin match
    decode backend (parse "backend {\n  kind udp\n  port 80\n}\n")
  with
  | Ok _ -> Test.check ~msg:"unknown tag rejected" false
  | Error (`Msg _) -> Test.check ~msg:"unknown tag rejected" true
  end

let test_cases_in_list =
  Test.test ~title:"cases in a list" @@ fun () ->
  let codec = list backend in
  let vs = [ Tcp 1; Unix "x"; Tcp 2 ] in
  let vs' = ok "decode" (decode codec (encode codec vs)) in
  Test.check ~msg:"list of unions round-trip" (vs = vs')

type user = { username : string; admin : bool }

let user =
  uniq
    (directive ~name:"user" (fun username admin -> { username; admin })
    |> req ~pos:0 string (fun u -> u.username)
    |> flag "admin" (fun u -> u.admin))

let test_flag =
  Test.test ~title:"flag (positional marker)" @@ fun () ->
  let decode_user s = ok "decode" (decode user (parse s)) in
  Test.check ~msg:"flag after"
    (decode_user "user hannes admin {\n}\n"
    = { username = "hannes"; admin = true });
  Test.check ~msg:"flag before"
    (decode_user "user admin hannes {\n}\n"
    = { username = "hannes"; admin = true });
  Test.check ~msg:"flag absent"
    (decode_user "user reynir {\n}\n" = { username = "reynir"; admin = false });
  let v = { username = "dinosaure"; admin = true } in
  Test.check ~msg:"decode/encode"
    (decode user (parse (render (encode user v))) = Ok v)

let () =
  Test.run
    [
      test_encode_decode;
      test_textual_roundtrip;
      test_optional_absent;
      test_missing_field;
      test_bad_int;
      test_recursive;
      test_cases;
      test_cases_in_list;
      test_flag;
    ]
