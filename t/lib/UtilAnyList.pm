package UtilAnyList;

use Util::Any::Light -Base;

my $utils;
sub utils {
  $utils = {
            -list   => [ qw/List::Util List::MoreUtils List::Pairwise/ ],
            -data   => [ qw/Scalar::Util/ ],
            -hash   => [ qw/Hash::Util/ ],
            -debug  => [ ['Data::Dumper', '', ['Dumper']] ],
            -scalar => [ qw/Scalar::Util/],
            -string => [ qw/String::CamelCase/ ],
           };
}

1;
