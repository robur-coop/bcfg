Test if we assume isomorphism when we encode(decode(cfg))
  $ bcfg iso<<EOF
  > foo
  > EOF
  $ bcfg iso<<EOF
  > foo bar
  > EOF
  $ bcfg iso<<EOF
  > foo bar {
  >   value
  > }
  > EOF
  $ bcfg iso<<EOF
  > foo "\x00bar\x00"
  > EOF
  $ bcfg iso --margin=10 - - <<EOF
  > foo a_long_bar
  > EOF
  foo "a_lon\
       g_bar"
  $ bcfg iso<<EOF
  > dinosaure {
  >   age 32
  >   website https://blog.osau.re/
  >   email din@osau.re
  > }
  > EOF
  $ bcfg iso<<EOF
  > foo { # bar
  >   bar # foo
  > }
  > EOF
  $ bcfg iso<<EOF
  > "#foo"
  > EOF
