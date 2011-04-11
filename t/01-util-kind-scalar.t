use Test::More qw/no_plan/;

use lib qw(t/lib);

use UtilAnyList '-scalar';
no strict 'refs';

foreach (@Scalar::Util::EXPORT_OK) {
  ok( defined &{$_} , $_);
}

foreach (@Hash::Util::EXPORT_OK) {
  no strict 'refs';
  ok(! defined &{$_} , $_) if defined &{'Hash::Util::' . $_};
}

foreach (@List::Util::EXPORT_OK) {
  ok(! defined &{$_} , $_);
}
foreach (@List::MoreUtils::EXPORT_OK) {
  ok(! defined &{$_} , $_);
}

