package UtilPluggable::Plugin::Pluggable2;

use strict;

sub utils {
  return
    {
     -pluggable2 => [
                     [
                      'UtilPluggable', '', # dummy,
                      {
                       "test3" => sub {sub (){ return "test3\n"}}
                      }
                     ]
                    ]
    }
}

1;


