# This -*- perl -*- script writes the Makefile for Sort::ArbBiLex
# Time-stamp: "1999-01-12 21:20:42 MST"
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	=> 'Sort::ArbBiLex',
    'VERSION_FROM' => 'ArbBiLex.pm', # finds $VERSION
    'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}

__END__
