# This -*- perl -*- script writes the Makefile for Sort::ArbBiLex
# Time-stamp: "2002-08-27 20:30:42 MDT"
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	=> 'Sort::ArbBiLex',
    'VERSION_FROM' => 'ArbBiLex.pm', # finds $VERSION
    'ABSTRACT_FROM' => 'ArbBiLex.pm', # 
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
