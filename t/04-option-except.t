use strict;

package UtilOption;

use Util::Any::Light -Base;
use strict;

sub utils {
  +{
    -list  => [
               [
                'List::Util', '',
                {
                 'first' => 'list_first',
                 'sum'   => 'lsum',
                 -except => ['reduce', 'shuffle'],
                }
               ]
              ],
   }
}

package test;

UtilOption->import(qw/-list/);
use Test::More qw/no_plan/;

ok(defined &list_first, 'defined first as list_first');
ok(defined &lsum, 'defined sum as lsum');
ok(defined &min, 'defined min');
ok(defined &minstr, 'defined minstr');
ok(!defined &reduce, 'not defined reduce');
ok(!defined &shuffle, 'not defined shuffle');
