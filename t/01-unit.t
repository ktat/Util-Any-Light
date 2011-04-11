use Test::More 'no_plan';
use lib qw(./lib t/lib);
use Util::Any::Light();
use exampleHello ();

is_deeply([sort @{Util::Any::Light::_all_funcs_in_class('exampleHello')}], [sort qw/hello_name hello_where/]);

my $r = Util::Any::Light ->_create_smart_rename("hoge");
is $r->("is_hoge"), "is_hoge";
is $r->("is_hogehoge"), "is_hogehoge";
is $r->("fuga"), "hoge_fuga";
is $r->("is_fuga"), "is_hoge_fuga";
is $r->("foo_bar_hoge"), "foo_bar_hoge";
is $r->("foo_bar_hoge_fuga"), "hoge_foo_bar_hoge_fuga";
