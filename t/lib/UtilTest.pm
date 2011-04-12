package UtilTest;

use strict;
use warnings;

use Util::Any::Light -Base;

sub utils {
  return {
          -test1 => [
                     [
                      UtilTest => {
                                   -no_redefine => 1,
                                   test => sub() { sub {'test1'}}
                                  },
                     ],
                     'List::MoreUtils',
                    ],
          -test2 => [
                     [
                      UtilTest => {
                                   -no_redefine => 1,
                                   test => sub() { sub {'test1'}}
                                  }
                     ],
                     'List::MoreUtils',
                    ],
         }
}

1;
