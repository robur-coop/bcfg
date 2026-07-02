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
Of course, on the other hand, `bcfg` does not enforce the use of a certain
indentation (an issue that has frustrated generations of developers).

### A semantic view: directives & parameters

`bcfg` is a very simple format where:
- only two elements are distinguished: directives and parameters
- only strings (UTF-8) are manipulated; there are no other "primary values"

A directive is like a shell command that can take zero or more arguments. A
directive consists of a 'name' and any associated parameters. All are in the
form of a UTF-8-encoded string. A directive may also have _children_, which are
defined within a block enclosed by `{` and `}`. Children are also directives,
which allows for a _recursive_ structure. A `bcfg` configuration file can
therefore be viewed as a tree of directives.

```
name parameter
---- ---------
user dinosaure {
  website https://din.osau.re/
}
```

The advantage of only manipulating strings as values is that it leaves the
interpretation of these values up to the user. For example, numbers can be
manipulated, but `bcfg` will only interpret them in the form in which they were
"injected" as strings. It is then up to the user to "project" these values into
what they expect (and God knows that this projection can be complicated: is this
number a floating point number? a large number? an `int64`? etc.). The only
constraint is that an unquoted value (not delimited by `'` or `"`) must comply
with UTF-8 encoding.

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

### Querying a configuration: `bcfg query`

`bcfg` also offers a simple query language inspired by `jq`. The aim is to
provide users with a way to extract information from a configuration file fairly
easily. Let's look at an example:

```cfg
server www.example.org {
  listen 443
  tls {
    certificate /etc/ssl/example.org.pem
  }
}

server git.example.org internal {
  listen 22
  listen 443
}

default git.example.org
```

Here are a few queries, along with the results they return:
```shell
$ bcfg query server.listen file.cfg
listen 443
listen 22
listen 443
$ bcfg query server("www.example.org").listen[0] file.cfg
443
$ bcfg query server("www.example.org"|internal).listen[0] file.cfg
443
22
443
$ bcfg query server(^internal).tls.certificate[0] file.cfg
/etc/ssl/example.org.pem
$ bcfg query server(@(default[0])).listen[0] file.cfg
22
443
$ bcfg query server(:tls).tls.certificate[0] file.cfg
/etc/ssl/example.org.pem
$ bcfg query server(^internal) file.cfg
server www.example.org {
  listen 443
  tls {
    certificate /etc/ssl/example.org.pem
  }
}
$ bcfg query server(:^tls)[0] file.cfg
git.example.org {
  listen 22
  listen 443
}
```

Our documentation covers every aspect of the query language if you wish, but
here is an overview of everything `bcfg` has to offer. It is also worth noting
that there are _several ways_ to retrieve specific information, which means you
are not restricted to a particular structure for your configuration files.

One option is to output JSON rather than the information in `bcfg` format; here
is an example that then allows you to use `jq` if you prefer:

```shell
$ bcfg query -o json server[0] file.cfg \
  | jq -r 'to_entries[]
           | [ .key, ([.value.listen] | flatten | join(" ")),
               (.value.tls.certificate // "no tls") ]
           | @csv'
"www.example.org","443","/etc/ssl/example.org.pem"
"git.example.org","22 443","no tls"
```

### Libraries and tooling

`bcfg` is an OCaml project that provides an executable for manipulating
configuration files, as well as a library that allows this to be done directly
in OCaml. OCaml has the advantage of being a language specifically designed to
manipulate, process and transform another language (`ml` stands for Meta
Language). Tools such as [Menhir][menhir] also provide a comprehensive overview
of the lexical and syntactic description of `bcfg`.

It is therefore possible to install `bcfg` via [OPAM] (the OCaml package
manager). The `bcfg` tool itself is available on our `apt.robur.coop`
repository:

```shell
$ opam install bcfg
$ curl -fsSL https://apt.robur.coop/gpg.pub \
  | gpg --dearmor | sudo tee /usr/share/keyrings/apt.robur.coop.gpg > /dev/null
$ echo "deb [signed-by=/usr/share/keyrings/apt.robur.coop.gpg] https://apt.robur.coop debian-13 main" \
  | sudo tee /etc/apt/sources.list.d/robur.list
$ sudo apt update
$ sudo apt install bcfg
```

### Tools

`bcfg` also provides three other tools for working with configuration files:
- `bcfg validate`, which validates a configuration file and can also explain any
  syntax errors
- `bcfg iso`, which verifies a fundamental assertion of our tool: it should not
  alter the contents of a configuration file and should output exactly the same
  result
- `bcfg lint`, which correctly indents an entire configuration file according to
  the user's preferences

### Libraries for OCaml developpers

Finally, there are several libraries available for working with configuration
files. You can essentially parse or generate configuration files, but what may
be of particular interest to OCaml developers is the `bcfgt` library (a library
inspired by the [`jsont`][jsont] project) which allows you to convert OCaml
values into configuration files and, conversely, to read configuration files and
extract OCaml values from them.

Here is an example of how to extract information from a configuration file in
OCaml:

```ocaml
type tls = { certificate : string }
type server = { hostname : string; listen : int list; tls : tls option }

let ( let* ) = Result.bind
let ( let@ ) finally fn = Fun.protect ~finally fn

let tls : tls Bcfgt.t =
  let open Bcfgt in
  directive ~name:"tls" (fun certificate -> { certificate })
  |> field "certificate" string (fun t -> t.certificate)
  |> uniq

let servers : server list Bcfgt.t =
  let open Bcfgt in
  directive ~name:"server" (fun hostname listen tls -> { hostname; listen; tls })
  |> req string (fun t -> t.hostname)
  |> field "listen" (list int) (fun t -> t.listen)
  |> opt "tls" tls ~get:(fun t -> t.tls)
  |> some

let run filepath =
  let ic = open_in filepath in
  let@ () = fun () -> close_in ic in
  let lexbuf = Lexing.from_channel ic in
  let* cfg = Bcfg.parser lexbuf in
  let* servers = Bcfgt.decode servers cfg in
  let show { hostname; listen; tls } =
    Fmt.pr "- %s (listening on %a)%a\n" hostname
      Fmt.(list ~sep:(any ", ") int) listen
      Fmt.(option ~none:nop (using (fun { certificate } -> certificate) (fmt " with %s"))) tls in
  List.iter show servers; Ok ()
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
[menhir]: https://cambium.inria.fr/~fpottier/menhir/
[opam]: https://opam.ocaml.org/
