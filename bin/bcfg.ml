open Cmdliner

let default =
  let open Term in
  const (`Help (`Pager, None)) |> ret

let cmd =
  let doc = "A simple tool to manipulate configuration files." in
  let man = [] in
  let info = Cmd.info "bcfg" ~doc ~man in
  Cmd.group ~default info
    [ Bcfg_validate.cmd; Bcfg_iso.cmd; Bcfg_query.cmd; Bcfg_lint.cmd ]

let () = Cmd.(exit (eval' cmd))
