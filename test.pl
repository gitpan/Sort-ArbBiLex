# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Sort::ArbBiLex;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

# $Sort::ArbBiLex::Debug = 2;

my $decl = [
 [' '],
 ['A', 'a'],
 ['b'],
 ["h", "x'"],
 ['i'],
 ['u'],
];
*foosort = Sort::ArbBiLex::maker($decl);

my $out = join(' ~ ',
 foosort(
  "ax'ub", 'ahuba', 'ahub iki', 'ahubiki', 'aba', 'Aba', 'hub', "x'ub"
 )
);
my $expected = "aba ~ Aba ~ ax'ub ~ ahub iki ~ ahuba ~ ahubiki ~ x'ub ~ hub";
print " Output  : $out\n Expected: $expected\n";
print $out eq $expected ? "ok 2\n" : "fail 2\n";

