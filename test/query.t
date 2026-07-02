A few queries on a small configuration.

  $ export BCFG_UTF_8=false
  $ cat >sample.cfg <<EOF
  > dinosaure github {
  >   website https://github.com/dinosaure
  > }
  > dinosaure gitlab {
  >   username dinosaure
  >   website https://din.osau.re/
  > }
  > hannes {
  >   website https://hannes.robur.coop/
  > }
  > EOF
  $ bcfg query "dinosaure.website" sample.cfg
  website https://github.com/dinosaure
  website https://din.osau.re/
  $ bcfg query "dinosaure[0]" sample.cfg
  github {
    website https://github.com/dinosaure
  }
  gitlab {
    username dinosaure
    website https://din.osau.re/
  }
  $ bcfg query -o json "dinosaure.username" sample.cfg
  {
    "username": "dinosaure"
  }
  $ bcfg query "(dinosaure|hannes).website" - < sample.cfg
  website https://github.com/dinosaure
  website https://din.osau.re/
  website https://hannes.robur.coop/
  $ bcfg query "(@(dinosaure.username)).website" sample.cfg
  website https://github.com/dinosaure
  website https://din.osau.re/
  $ cat >servers.cfg <<EOF
  > server www.example.org {
  >   listen 443
  >   tls {
  >     certificate /etc/ssl/example.org.pem
  >   }
  > }
  > server git.example.org internal {
  >   listen 22
  >   listen 443
  > }
  > default git.example.org
  > EOF
  $ bcfg query "server('www.example.org').listen[0]" servers.cfg
  443
  $ bcfg query 'server("git.example.org").listen[0]' servers.cfg
  22
  443
  $ bcfg query "'server'(:^tls)[0]" servers.cfg
  git.example.org {
    listen 22
    listen 443
  }
  $ bcfg query "*.listen(443)[0]" servers.cfg
  443
  443
  $ bcfg query "server('www" servers.cfg
  bcfg: Unterminated quote in the query: "server('www"
  [124]
  $ bcfg query "foo[9999999999999999999999]" servers.cfg
  bcfg: Invalid index "9999999999999999999999" in the query
  [124]
  $ bcfg query "foo[bar]" servers.cfg
  bcfg: Invalid index "bar" in the query
  [124]
