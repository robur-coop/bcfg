# `bcfg`, a simple configuration format for OCaml

The `bcfg` format is fairly simple and enforces the use of new lines. Here is an
example of a configuration that complies with the format. This format is very
much based on the format described [here][scfg-format].
```bcfg
dinosaure {
  age 34
  email din@osau.re
  website https://blog.osau.re
}
```

It is said that `bcfg` is _line-driven_ in the sense that the format forces the
user to insert line breaks. For example, it is not possible to write
`dinosaure { foo }`; the user must insert a line break after `{` and before `}`.
Of course, on the other side, `bcfg` does not enforce the use of a certain
indentation (as this issue can frustrate generations of developers).

### A semantic view: directives & parameters

`bcfg` is a very simple format where:
- only two elements are distinguished: directives and parameters
- only strings (UTF-8) are manipulated; there are no other "primary values"

The advantage of only manipulating strings as values is that it leaves the
interpretation of these values up to the user. For example, numbers can be
manipulated, but `bcfg` will only interpret them in the form in which they were
"injected" as strings. It is then up to the user to "project" these values into
what they expect (and God knows that this projection can be complicated: is this
number a floating point number? a large number? a `int64`? etc.). The only
constraint is that a value without quotation (like `'` or `"`) must comply with
UTF-8 encoding.

Next comes the interpretation that can be made of the directives and parameters.
As such, it is important to understand the purpose of a configuration file. It
allows information to be stored that can be "characterised" and retrieved using
this characterisation. A fairly spontaneous characterisation consists of
associating a "key" with values: a key that crystallises a (tacit?) agreement
between the developer and the user as a good characterisation of the value:
```cfg
username dinosaure
```

However, a value can also be characterised in several ways. In other words, a
value can correspond to several pieces of information required by the
application:
```cfg
dinosaure as_github_username as_login
```

If you consider our two examples, it is clear that a directive can be a value
and parameters can also be values. `bcfg` does not force a particular
interpretation of these elements and leaves it up to the user and developer to
define how to interpret these two elements.

Finally, if we consider these two interpretations, that the directive is the
characterisation of a value, it can be supplemented by other sub-directives.
But if the directive is the value, it can be supplemented by other
sub-directives:
```cfg
username dinosaure {
  website https://blog.osau.re/
  email din@osau.re
}

dinosaure as_github_account at_robur {
  website https://blog.osau.re/
  email din@osau.re
}
```

In relation to this introduction, the question now is whether such
interpretations of the configuration file can be easily described in a
programming language. This is where OCaml comes in, offering developers a DSL
that is expressive enough for these types of interpretations.

### Libraries and tooling

The distribution provides several layers:

- `bcfg`: the core parser (`Bcfg.parser`) and lazy emitter (`Bcfg.emitter`),
  with precise error localisation.
- `Bcfg.Stream`: a fully streaming, SAX-like API. A configuration is processed
  as a flat sequence of _lexemes_, both for decoding and encoding. The memory
  footprint is bounded by the nesting depth, not by the size of the input, which
  makes it suitable for very large files.
- `bcfgt`: a [`jsont`][jsont]-like, bidirectional combinator API to read and
  write OCaml values. A `'a Bcfgt.t` is a codec: it can decode a configuration
  into an `'a` and encode an `'a` back into a configuration. It offers
  scalars value according to primary OCaml types (such as `int`) and some
  combinators.
- `bcfg` (the command-line tool): `validate`, `iso`, `query` and `lint`. The
  `query` sub-command is a small `jq`-like selector (it even supports `@(...)`
  substitutions -- also written `$(...)` --, e.g. `(@(me.username))`); its
  result can be printed as `bcfg` (the default) or as JSON with `-o json`, ready
  to be piped into `jq`. The `lint` sub-command reformats a configuration: it
  re-indents it and wraps long values at the right margin. The indentation and
  the margin are configurable in the spirit of `vim`'s `shiftwidth`/`expandtab`
  and `textwidth`: `--indent N` for `N` spaces, `--indent tab` for tab
  characters (with `--tab-width` as the tab stop), and `--margin C`.

```sh
$ bcfg query 'dinosaure.website' config.cfg
$ bcfg query -o json 'dinosaure.username' config.cfg | jq .
$ bcfg lint --indent tab --margin 100 -i config.cfg
```

### Note about `scfg` (and its OCaml implementation)

The project arose from the idea of having a 'simple' configuration file. The
`scfg` format provides a good foundation, and the project initially began as an
effort to improve [`scfg`][scfg-ocaml] so that it uses `ocamllex` rather than
`sedlex` and offers an interface similar to [`jsont`][jsont] for decoding and
encoding configuration files in OCaml.

As the work involved was quite substantial, I decided to make it a project in
its own right, even though the credit goes to Léo Andrès, the original author
of `scfg`. Finally, the format has been extended, notably with the ability to
"wrap" values according to [RFC 822][rfc822] (drawing on my skills acquired in
email processing). This extension is **not** part of the `scfg` format.

[scfg-format]: https://git.sr.ht/~emersion/scfg
[jsont]: https://github.com/dbuenzli/jsont
[scfg-ocaml]: https://forge.kumikode.org/kumikode/scfg
[rfc822]: https://www.rfc-editor.org/info/rfc822/
