Examples on the README.md
  $ cat >foo.cfg<<EOF
  > server www.example.org {
  >   listen 443
  >   tls {
  >     certificate /etc/ssl/example.org.pem
  >   }
  > }
  > 
  > server git.example.org internal {
  >   listen 22
  >   listen 443
  > }
  > 
  > default git.example.org
  > EOF
  $ bcfg query server.listen foo.cfg
  listen 443
  listen 22
  listen 443
  $ bcfg query 'server("www.example.org").listen[0]' foo.cfg
  443
  $ bcfg query 'server("www.example.org"|internal).listen[0]' foo.cfg
  443
  22
  443
  $ bcfg query 'server(^internal).tls.certificate[0]' foo.cfg
  /etc/ssl/example.org.pem
  $ bcfg query 'server(@(default[0])).listen[0]' foo.cfg
  22
  443
  $ bcfg query 'server(:tls).tls.certificate[0]' foo.cfg
  /etc/ssl/example.org.pem
  $ bcfg query 'server(^internal)' foo.cfg
  server www.example.org {
    listen 443
    tls {
      certificate /etc/ssl/example.org.pem
    }
  }
  $ bcfg query 'server(:^tls)[0]' foo.cfg
  git.example.org {
    listen 22
    listen 443
  }
