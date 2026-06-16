A simple test to see what we can validate or not
  $ export BCFG_UTF_8=false
  $ bcfg validate<<EOF
  > EOF
  $ bcfg validate<<EOF
  > foo
  > EOF
  $ bcfg validate<<EOF
  > foo {
  > bar }
  > EOF
  Error at l2.4-5:
  > The "bar" directive must end with a line break or an opening curly bracket.
   1 foo {
  >2 bar }\n
  [1]
  $ bcfg validate<<EOF
  > foo {
  > EOF
  Error at l2.0-0:
  > Missing a subdirective.
   1 foo {
  >  EOF
  [1]
  $ bcfg validate<<EOF
  > dinosaure {
  >   age 32
  >   website https://blog.osau.re/
  >   email din@osau.re
  > }
  > EOF
  $ bcfg validate<<EOF
  > # foo
  > EOF
  $ bcfg validate<<EOF
  > foo {
  >  bar
  > EOF
  Error at l3.0-0:
  > We are in a directive that does not end; a closing curly bracket should follow the subdirective "bar".
   2  bar
  >  EOF
  [1]
  $ bcfg validate<<EOF
  > 
  > }
  > EOF
  Error at l2.0-1:
  > Unexpected closing curly bracked.
   1 
  >2 }\n
  [1]
  $ bcfg validate<<EOF
  > foo bar }
  > EOF
  Error at l1.8-9:
  > Unexpected closing curly bracket after "bar".
  >1 foo bar }\n
  [1]
  $ bcfg validate<<EOF
  > foo
  > {
  > bar
  > }
  > EOF
  Error at l2.0-1:
  > A directive (potentially with parameters) must always precede a opening curly bracket.
   1 foo
  >2 {\n
   3 bar
  [1]
  $ bcfg validate<<EOF
  > foo { bar
  > EOF
  Error at l1.6-9:
  > A opening curly bracket must always be followed by a line break.
  >1 foo { bar\n
  [1]
  $ bcfg validate<<EOF
  > foo {
  >  bar
  > } dinosaure
  > EOF
  Error at l3.2-11:
  > A closing curly bracket must always be followed by a line break.
   2  bar
  >3 } dinosaure\n
  [1]
  $ bcfg validate<<EOF
  > foo
  > }
  > EOF
  Error at l2.0-1:
  > The directive "foo" was never opened, so there is no reason to close it.
   1 foo
  >2 }\n
  [1]
  $ bcfg validate < materials/valid/atom_escaped_chars.scfg
  $ bcfg validate < materials/valid/blocks_nested.scfg
  $ bcfg validate < materials/valid/comment_preceding_ws.scfg
  $ bcfg validate < materials/valid/comment_preceding_ws2.scfg
  $ bcfg validate < materials/valid/curly_brace_names.scfg
  $ bcfg validate < materials/valid/curly_brace_names2.scfg
  $ bcfg validate < materials/valid/curly_brace_names3.scfg
  $ bcfg validate < materials/valid/curly_brace_names4.scfg
  $ bcfg validate < materials/valid/dq_escape.scfg
  $ bcfg validate < materials/valid/dq_with_sq.scfg
  $ bcfg validate < materials/valid/empty.scfg
  $ bcfg validate < materials/valid/empty2.scfg
  $ bcfg validate < materials/valid/empty_block.scfg
  $ bcfg validate < materials/valid/empty_block2.scfg
  $ bcfg validate < materials/valid/empty_block3.scfg
  $ bcfg validate < materials/valid/empty_param.scfg
  $ bcfg validate < materials/valid/empty_param2.scfg
  $ bcfg validate < materials/valid/escape_seq.scfg
  $ bcfg validate < materials/valid/escape_ws_at_eol.scfg
  $ bcfg validate < materials/valid/example.scfg
  $ bcfg validate < materials/valid/example2.scfg
  $ bcfg validate < materials/valid/hash_as_param.scfg
  $ bcfg validate < materials/valid/param_spaces.scfg
  $ bcfg validate < materials/valid/param_spaces2.scfg
  $ bcfg validate < materials/valid/quoted_name_and_params.scfg
  $ bcfg validate < materials/valid/same_name.scfg
  $ bcfg validate < materials/valid/sq_backslash.scfg
  $ bcfg validate < materials/valid/sq_backslash2.scfg
  $ bcfg validate < materials/valid/unicode_name.scfg
  $ bcfg validate < materials/valid/unicode_params.scfg
  $ bcfg validate < materials/valid/whitespace_after_brace.scfg
