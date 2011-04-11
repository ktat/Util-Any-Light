package UtilPluggable;

use UtilAnyList -Base, -Pluggable;

sub utils {
  +{
    -parent => [
                [
                 "String::CamelCase", '',
                 ["camelize"],
                ],
               ]
   };
}
1;
