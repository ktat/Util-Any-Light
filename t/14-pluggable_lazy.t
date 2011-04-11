use Test::More qw/no_plan/;
use Class::Unload qw/unload_class/;
use lib qw(./lib t/lib);

require UtilPluggable;

package BBB;

UtilPluggable->import(-pluggable2);
use Test::More;

ok(!defined &test);
ok(!defined &test2);
ok(!defined &camelize);
ok(!defined &xxx);
ok(defined &test3);

main::unload();

package AAA;

UtilPluggable->import(-pluggable_all);
use Test::More;

ok(defined &test);
ok(defined &test2);
ok(!defined &camelize);
ok(!defined &test3);
ok(defined &xxx);

main::unload();

package CCC;

use Test::More;
UtilPluggable->import(-pluggable, -pluggable2);

ok(defined &test);
ok(defined &test2);
ok(!defined &camelize);
ok(defined &test3);
ok(!defined &xxx);

main::unload();

package DDD;

use Test::More;
UtilPluggable->import(-all);

ok(defined &test);
ok(defined &test2);
ok(defined &camelize);
ok(defined &test3);
ok(defined &xxx);

main::unload();

package EEE;

use Test::More;
UtilPluggable->import(-pluggable_xxx);

ok(!defined &test);
ok(!defined &test2);
ok(!defined &camelize);
ok(!defined &test3);
ok(defined &xxx);

main::unload();

package main;

sub unload {
  Class::Unload->unload($_) for qw{UtilPluggable::Plugin::Pluggable UtilPluggable::Plugin::Pluggable2 UtilPluggable UtilPluggable::Plugin::Pluggable::XXX};
  require UtilPluggable;
}
