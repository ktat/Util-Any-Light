use Test::More qw/no_plan/;

use lib qw(./lib t/lib);
use Class::Unload;
no warnings qw/once/;

require UtilPluggable;
{
  package BBB;
  UtilPluggable->import(-pluggable2, {plugin => 'eager'});
  use Test::More;
  ok( defined &UtilPluggable::Plugin::Pluggable2::utils, 'utils of Pluggable2');
  ok( defined &UtilPluggable::Plugin::Pluggable::utils,  'utils of Pluggable');
  ok(!defined &test, "test is in Pluggbale, not Pluggable2");
  ok(!defined &test2,  "test2 is in Pluggable, not Pluggable2");
  ok(!defined &camelize, "parent has camelize, but not imported");
  ok(defined &test3, "test3 is in Pluggable2");
}

{
  package AAA;
  UtilPluggable->import(-pluggable, {plugin => 'eager'});
  use Test::More;

  ok(defined &test,     'test test is in Pluggable');
  ok(defined &test2,   'test2 is in Pluggable');
  ok(!defined &camelize, 'parent has camelize, but not imported');
  ok(!defined &test3,   'test3 is in Pluggable2');
}
{
  package CCC;
  use Test::More;
  UtilPluggable->import(-pluggable, -pluggable2, {plugin => 'eager'});

  ok(defined &test    , 'test is in Pluggable');
  ok(defined &test2   , 'test2 is in Pluggable');
  ok(!defined &camelize, 'parent has camelize, but not imported');
  ok(defined &test3,    'test3 is in Pluggable2');
}
{
  package DDD;
  use Test::More;
  UtilPluggable->import(-all, {plugin => 'eager'});

  ok(defined &test);
  ok(defined &test2);
  ok(defined &camelize);
  ok(defined &test3);
}

