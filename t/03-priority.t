use Test::More qw/no_plan/;
use strict;
use lib qw(./lib t/lib);
require UtilAnyList;

no warnings 'once';

my $__utils = UtilAnyList->utils;
$__utils->{-list} = [
                   ["List::Util", "LLLL"],
                   ["List::MoreUtils", "llll"],
                  ];

package AAA;
use Test::More;

UtilAnyList->import(-list => {uniq => {-as => 'luniq'}, -prefix => "ll"}, {prefix => 1, smart_rename => 1});
ok(defined &luniq);

package BBB;
use Test::More;

UtilAnyList->import(-list => ['uniq', -prefix => "ll"], {prefix => 1, smart_rename => 1});

ok(defined &lluniq);

package CCC;
use Test::More;

UtilAnyList->import(-list => ['uniq'], {prefix => 1, smart_rename => 1});

ok(! defined &lllluniq);

package DDD;
use Test::More;

UtilAnyList->import(-list => ['uniq'], {prefix => 1, smart_rename => 1});

ok(defined &list_uniq);

package EEE;
use Test::More;

UtilAnyList->import(-list => ['uniq'], {smart_rename => 1});

ok(! defined &lllluniq);

package FFF;
use Test::More;

UtilAnyList->import(-list => ['uniq'], { prefix => 1});

ok(! defined &lllluniq);

1;
