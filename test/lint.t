  $ export BCFG_UTF_8=false
  $ cat >messy.cfg <<EOF
  > foo      bar {
  > child value
  > nested {
  > deep "a quite long value that is definitely going to exceed the small margin we will ask for"
  > }
  > }
  > EOF
  $ bcfg lint messy.cfg
  foo bar {
    child value
    nested {
      deep "a quite long value that is definitely going to exceed the small margin\
            \x20we will ask for"
    }
  }
  $ bcfg lint --indent 4 --margin 40 messy.cfg
  foo bar {
      child value
      nested {
          deep "a quite long value that is\
                \x20definitely going to ex\
                ceed the small margin we w\
                ill ask for"
      }
  }
  $ bcfg lint -i messy.cfg
  $ bcfg lint messy.cfg | diff - messy.cfg
  $ cat >nested.cfg <<EOF
  > a {
  > b {
  > c value
  > }
  > }
  > EOF
  $ bcfg lint --indent tab nested.cfg
  a {
  	b {
  		c value
  	}
  }
