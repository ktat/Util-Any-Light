use Test::More qw/no_plan/;
use lib qw(t/lib);

use UtilAnyList qw/All/, {prefix => 1};
no strict 'refs';

foreach (@List::Util::EXPORT_OK) {
  ok(defined &{'list_' . $_} , $_);
}
foreach (@List::MoreUtils::EXPORT_OK) {
  ok(defined &{'list_' . $_} , $_);
}

foreach (@Scalar::Util::EXPORT_OK) {
  ok(defined &{'scalar_' . $_} , $_);
}

foreach (@Hash::Util::EXPORT_OK) {
  no strict 'refs';
  ok(defined &{'hash_' . $_} , $_) if defined &{'Hash::Util::' . $_};
}
