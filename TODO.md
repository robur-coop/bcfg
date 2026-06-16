- [x] a really good parser with nice errors
  + [x] here, we must strictly localize errors and show them to the user (in a nice way)
  + [x] we should be able to work with `stdin` or a file
  + [x] verify UTF-8
  + [x] implement a validator from `ocamllex` and `menhir`
- [x] a fully streaming (SAX-like) API with a bounded memory footprint
      (see `Bcfg.Stream`: lexemes, pull decoder, push encoder)
- [x] lint a configuration file (see `bcfg lint`)
  + [x] outputs a configuration file without comments
  + [x] re-indents (spaces or tabs, "vim"-like) and wraps long values at a margin
  + [ ] outputs it with colors?
- [x] be able to transform a configuration file to an OCaml value (see `jsont`)
      (see `bcfgt`: a bidirectional combinator codec)
- [x] export a configuration value to:
  + [x] an OCaml value (via `bcfgt`)
  + [x] a JSON value (via `Bcfg_json`, `bcfg query -o json`)
- [x] implement something like `jq` to analyze a configuration file
  + [x] like filter (see `bcfg query`)

foo(for_me|for_my_parents) : directive list => means match all foo directives
  with the parameters "for_me" OR "for_my_parents"

```cfg
foo for_me {
  username dinosaure
  email din@osau.re
}

foo for_my_parents {
  username dinosaure
  email romain.calascibetta@gmail.com
}
bar
```

[ { foo; [ for_me ]; [ username; password ]}
; { foo; [ for_my_parents ]; [ username; password ]} ]

foo.bar : directive (list?) => return the directive "bar" which should exist
  into "foo"

implictly, foo.bar <=> foo.bar[0]
  => foo.bar : string if we consider this equivalence
     no, at top, we keep the 'directive list' for the user but we have such
     equivalence when we eval (with $(foo.bar) for instance)

foo(&for_me).username : directive (list?) => return the directive "username"
  which should exist into "foo" a match with "for_me"

( and ) specifies a set of parameters with a specific operator (| or &)
  or contains a boolean expression?
- (a|b)
- (|a,b,c)
- (!(a|b))
- (!(&,a,b,c))
- (!a)
- (a|b&c)

Now, can we match on directives?

(foo|bar)[0] : string => take the first parameter of directives which pattern on
  "foo" or "bar"

Can we compose?

foo(&,$(bar.username),dinosaure) : directive list =>
  

foo.(foo|bar)[0]

foo.(bar) => foo { bar value } => value

