package UtilPluggable::Plugin::Pluggable;
use strict;

sub utils {
  return
    {
     -pluggable => [
                    [
                     'UtilPluggable', # dummy,
                     {
                      "test2" => sub {sub (){ return "test2\n"}},
                      "test" => sub {sub (){ return "test\n"}}
                     }
                    ]
                   ],
    }
}

1;
