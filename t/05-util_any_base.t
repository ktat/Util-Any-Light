use Test::More qw/no_plan/;

use lib qw(./lib ./t/lib);
use List::Util ();
use List::MoreUtils ();
use Hash::Util ();
use Scalar::Util ();

use MyUtilBase qw/List/;
no strict 'refs';

foreach (@List::Util::EXPORT_OK) {
  ok(defined &{$_} , $_);
}

foreach (@List::MoreUtils::EXPORT_OK) {
  ok(! defined &{$_} , $_);
}

foreach (@Scalar::Util::EXPORT_OK) {
  ok(! defined &{$_} , $_);
}

foreach (@Hash::Util::EXPORT_OK) {
  no strict 'refs';
  ok(! defined &{$_} , $_) if defined &{'Hash::Util::' . $_};
}

