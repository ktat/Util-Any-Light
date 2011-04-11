use Test::More qw/no_plan/;

use lib qw(t/lib);
use UtilAnyList -debug;
use strict;

ok(defined &Dumper , 'defined &Dumper');
ok(!defined &DumeprX , 'not defined DumperX');
