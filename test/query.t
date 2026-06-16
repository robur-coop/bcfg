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
  $ bcfg query "(\$(dinosaure.username)).website" sample.cfg
  website https://github.com/dinosaure
  website https://din.osau.re/
