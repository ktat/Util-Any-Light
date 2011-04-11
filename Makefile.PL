use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Util::Any::Light',
    AUTHOR              => 'Ktat <ktat@cpan.org>',
    VERSION_FROM        => 'lib/Util/Any/Light.pm',
    ABSTRACT_FROM       => 'lib/Util/Any/Light.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Clone'             => 0,
        'Class::Load'       => 0,
	'Data::Util'        => 0,
	'Module::Pluggable' => 0,
    },
    BUILD_REQUIRES => {
        'Test::More'        => 0,
        'Class::Unload'     => 0,
	'List::Util'        => 0,
	'List::MoreUtils'   => 0,
	'Scalar::Util'      => 0,
	'Hash::Util'        => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Util-Any-Light-*' },
);
