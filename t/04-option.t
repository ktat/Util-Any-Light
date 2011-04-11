use strict;

package UtilOption;

use Util::Any::Light -Base;
use strict;

sub utils {
  +{
    debug  => [
               [
                'Data::Dumper', '',
                {'Dumper' => 'dumper',
                }
               ]
              ],
   };
}

UtilOption->import(qw/debug/);
use Test::More qw/no_plan/;

ok(defined &dumper, 'defined Dumepr as dumper');
ok(!defined &Dumper, 'not defined Dumper');
ok(defined &DumperX, 'defined DumperX');
my $d = dumper({x => 1});
my $VAR1;
eval "$d";
is_deeply($VAR1, {x => 1}, 'dumper is Dumper');
