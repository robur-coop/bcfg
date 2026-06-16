Tests about invalid configuration files

A few 'invalid' examples of scfg files work with bcfg. They are listed at the
end of this file. In principle, it is acceptable for bcfg to accept such files.

What is important is to note that bcfg is able to explain all errors.

  $ export BCFG_UTF_8=false
  $ bcfg validate < materials/invalid/block_not_closed.scfg
  Error at l4.0-0:
  > We are in a directive that does not end; a closing curly bracket should follow the subdirective "child".
   3 
  >  EOF
  [1]
  $ bcfg validate < materials/invalid/block_not_closed_nested.scfg
  Error at l5.0-0:
  > We are in a directive that does not end; a closing curly bracket should follow the subdirective "child".
   4 
  >  EOF
  [1]
  $ bcfg validate < materials/invalid/block_without_directive_name.scfg
  Invalid syntax at l1.0-1:
  >1 {\n
   2 	child
  [1]
  $ bcfg validate < materials/invalid/block_without_newline.scfg
  Error at l1.7-9:
  > A opening curly bracket must always be followed by a line break.
  >1 test { me }\n
  [1]
  $ bcfg validate < materials/invalid/block_without_newline2.scfg
  Error at l2.5-6:
  > The "me" directive must end with a line break or an opening curly bracket.
   1 test {
  >2   me }\n
   3 
  [1]
  $ bcfg validate < materials/invalid/dq_end_of_word.scfg
  Invalid character "\n" at l1.10-11:
  >1 directive"\n
   2 
  [1]
  $ bcfg validate < materials/invalid/dq_not_closed.scfg
  Invalid character "\n" at l1.19-20:
  >1 directive "unclosed\n
   2 
  [1]
  $ bcfg validate < materials/invalid/escape_end_of_word.scfg
  Invalid character "\\" at l1.9-10:
  >1 directive\\n
   2 
  [1]
  $ bcfg validate < materials/invalid/sq_end_of_word.scfg
  Invalid character "\n" at l1.10-11:
  >1 directive'\n
   2 
  [1]
  $ bcfg validate < materials/invalid/sq_not_closed.scfg
  Invalid character "\n" at l1.19-20:
  >1 directive 'unclosed\n
   2 
  [1]
  $ bcfg validate < materials/invalid/comment_after_brace.scfg
  $ bcfg validate < materials/invalid/escape_within_sq.scfg
  $ bcfg validate < materials/invalid/sq_escape.scfg
