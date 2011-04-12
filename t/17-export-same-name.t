use Test::More;

use lib qw(./t/lib);
use UtilTest -all;

is(test, "test1", "dummy test");
is(uniq (1,2,3,2)  , 3, "dummy test");

done_testing;
