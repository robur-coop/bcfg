let () = Printexc.record_backtrace true
let passed = ref 0
let failed = ref 0

let check ?(msg = "") test =
  if test then begin
    incr passed;
    print_char '.'
  end
  else begin
    incr failed;
    print_char 'x';
    if msg <> "" then Format.printf "@.  [fail] %s" msg
  end;
  flush stdout

type t = { title : string; fn : unit -> unit }

let test ~title fn = { title; fn }

let run tests =
  let one { title; fn } =
    Format.printf "%-34s " title;
    flush stdout;
    (try fn ()
     with exn ->
       incr failed;
       Format.printf "@.  [exn] %s@.%s" (Printexc.to_string exn)
         (Printexc.get_backtrace ()));
    Format.printf "@."
  in
  List.iter one tests;
  Format.printf "@.%d passed, %d failed@." !passed !failed;
  if !failed > 0 then exit 1
