#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Util::Any::Light ' );
}

diag( "Testing Util::Any::Light $Util::Any::Light::VERSION, Perl $], $^X" );
