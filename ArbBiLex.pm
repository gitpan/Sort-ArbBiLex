
# -*-Fundamental-*- 
require 5;    # Time-stamp: "2000-12-09 01:15:50 MST"
package Sort::ArbBiLex;
use strict;
use vars qw(@ISA $Debug $VERSION);
$VERSION = 3.31;
$Debug = 0;
use Carp;

#POD at end 
###########################################################################

sub import {
  my $class_name = shift(@_);
  my $into = scalar caller;
  return unless @_;
  croak "Argument list in 'use $class_name' must be list of pairs" if @_ % 2;
  my($sym, $spec);
  while(@_) {
    ($sym, $spec) = splice(@_,0,2);
    defined $sym  or croak "Can't use undef as the name of a sub to make";
    length $sym   or croak "Can't use \"\" as the name of a sub to make";
    defined $spec or croak "Can't use undef as a sort-order spec";
    length $sym   or croak "Can't use \"\" as a sort-order spec";
    $sym = $into . '::' . $sym unless $sym =~ m/::/ or $sym =~ m/'/;
    no strict 'refs';
    *{$sym} = maker($spec);
  }
  return;
}

#--------------------------------------------------------------------------

sub maker {
  my $subr = eval(&source_maker(@_));
  die "Compile error <$@> in eval!?!" if $@; # shouldn't be possible!
  return $subr;
}

# Implementation note:  I didn't /need/ to use eval().  I could just return
#  an appropriate closure.  But one can't do tr/$foo/$bar/ -- eval is the
#  only way to get things to (so to speak) interpolate there; and the
#  efficiency cost of requiring that Perl parse more code is offset by
#  the efficiency benefit of being able to use tr/// (instead of s///) in
#  appropriate cases.

#--------------------------------------------------------------------------

sub source_maker {
  no locale;
  my($decl) = $_[0];
  croak "usage: Sort::ArbBiLex::maker(DECLARATION).  See the docs."
   unless @_ == 1;

  my $one_level_mode = 0;
  my @decl;
  if(ref $decl) { # It's a rLoL declaration
    croak "Sort order declaration must be a string or a listref"
      unless ref($decl) eq 'ARRAY';
    print "rLoL-decl mode\n" if $Debug > 1;
    # Make @decl into a list of families
    @decl = @$decl;
     # and each one of the items in @decl must be a ref to a list of scalars
    foreach my $f (@decl) {
      croak "Each family must be a listref" unless ref($f) eq 'ARRAY';
      @$f = grep(defined($_) && length($_), @$f); # sanity
      foreach my $g (@$f) { # more sanity.
        croak "A reference found where a glyph was expected" if ref($g);
      }
    }

  } else { # It's a string-style declaration
    print "string-decl mode\n" if $Debug > 1;
    # Make @decl into a list of families
    if($decl =~ /[\cm\cj\n]/) { # It contains majors and minors
      @decl = grep /\S/, split( /[\cm\cj]+/, $decl );
    } else { # It's all majors, on one line
      print "Strangeness trap 1.\n" if $Debug;
      @decl = grep /\S/, split( /\s+/, $decl );
      $one_level_mode = 1;
    }

    # Now turn @decl into a list of lists, where each element is a
    #  family -- i.e., a ref to a list of glyphs in that family.

    print "Glyph map:\n", map("  {<$_>}\n", @decl) if $Debug > 1;
    foreach my $d (@decl) { # in place changing
      #print "  d $d -> ", map("<$_> ",grep($_ ne '',split(/\s+/, $d))), "\n";
      $d = [ grep($_ ne '', split(/\s+/, $d)) ];
      #print "  d $d -> ", map("<$_> ", @$d), "\n";
    }
  }

  @decl = grep( scalar(@{$_}), @decl); # nix empty families
  croak "No glyphs in sort order declaration!?" unless @decl;

  @decl = map [$_], @{$decl[0]}  if  @decl == 1;
   # Change it from a family of N glyphs into N families of one glyph each
  
  # Iterate thru the families and their glyphs and build the tables
  my(@glyphs, @major_out, @minor_out);
  my $max_glyph_length = 0;
  my $max_family_length = 0;
  my %seen;
  my($glyph, $minor); # scratch
  for (my $major = 0; $major < @decl; $major++) {
    print "Family $major\n" if $Debug;
    croak "Too many major glyphs" if $major > 255;
    $max_family_length = @{ $decl[$major] }
     if  @{ $decl[$major] }  >  $max_family_length;

    for ($minor = 0; $minor < @{ $decl[$major] }; $minor++) {
      $glyph = $decl[$major][$minor];
      print "  Glyph ($major)\:$minor (", $glyph, ")\n" if $Debug;
      croak "Glyph <$glyph> appears twice in the sort order declaration!"
       if $seen{$glyph}++;
      croak "Too many minor glyphs" if $minor > 255;

      $max_glyph_length = length($glyph) if length($glyph) > $max_glyph_length;

      $glyph =~ s/([^a-zA-Z0-9])/'\\x'.(unpack("H2",$1))/eg;
      push @glyphs,    $glyph;
       # was: push @glyphs,    "\\x" . unpack('H2', $decl[$major][$minor] );
      push @major_out, sprintf "\\x%02X", $major;
      push @minor_out, sprintf "\\x%02X", $minor;
        #  or  unpack 'H2', pack 'C', 12   or   unpack 'H2', chr 12; ?
    }
  }
  die "Unexpected error: No glyphs?!?" if $max_glyph_length == 0;  # sanity
  $one_level_mode = 1 if $max_family_length == 1;

  #########################################################################
  # Now start building the code.

  my($prelude, $coda, $code, $minor_code, $major_code);
  if($max_glyph_length == 1) {
    # All glyphs are single characters, so we can do this all with tr's
    $prelude = "# Single character mode.";
    $coda = '';
    my $glyphs    = join '', @glyphs;
    my $major_out = join '', @major_out;
    my $minor_out = join '', @minor_out;

    $minor_code = <<"EOMN"; # contents of a FOR block mapping $$x[0] => $$x[2]
                 \$x->[2] =  \$x->[0];
                 \$x->[2] =~ tr[$glyphs][]cd;
                 \$x->[2] =~ tr[$glyphs]
                             [$minor_out];
EOMN

  $major_code = <<"EOMJ"; # expression returning a scalar as a major key
            do { # major keymaker
               my(\$key) = \$_;
               \$key =~ tr[$glyphs][]cd;
               \$key =~ tr[$glyphs]
                         [$major_out];
               scalar(\$key);
            }
EOMJ

    # End of single-glyph stuff.

  } else {
    # There are glyphs over 2 characters long -- gotta use s's.
    # End of multi-glyph stuff.
    my $glyphs    = join ',', map "\"$_\"", @glyphs;
    my $major_out = join ',', map "\"$_\"", @major_out;
    my $minor_out = join ',', map "\"$_\"", @minor_out;

    if(!$one_level_mode) {
      $prelude = <<"EOPRELUDE";
{ # Multi-character mode.  So we need a closure for these variables.
my(\%major, \%minor);
\@major{$glyphs}
 = ($major_out);
\@minor{$glyphs}
 = ($minor_out);
my \$glyph_re = join "|", map(quotemeta,
                             sort {length(\$b) <=> length(\$a)} keys \%major);
                             # put the longest glyphs first
EOPRELUDE
    } else { #  Multi-character mode
      $prelude = <<"EOPRELUDE2";
{ # Multi-character mode.  So we need a closure for these variables.
my(\%major); # just one-level mode, tho.
\@major{$glyphs}
 = ($major_out);
my \$glyph_re = join "|", map(quotemeta,
                             sort {length(\$b) <=> length(\$a)} keys \%major);
                             # put the longest glyphs first
EOPRELUDE2
}
    $coda = "} # end of closure.";

    $minor_code = <<"EOMN2"; # contents of a FOR block mapping $$x[0] => $$x[2]
                 \$x->[2] = join '',
                                map \$minor{\$_},
                                    \$x->[0] =~ m<(\$glyph_re)>go;
EOMN2

  $major_code = <<"EOMJ2"; # expression returning a scalar as a major key
            join('', map \$major{\$_}, m<(\$glyph_re)>go)  # major keymaker
EOMJ2

  }

  ### 
  # Now  finish cobbling the code together.

  my $now = scalar(gmtime);

if(!$one_level_mode) {  # 2-level mode
  $code = <<"EOVOODOO";
\# Generated by Sort::ArbBiLex v$VERSION at $now GMT
$prelude
# Two-level mode
sub {   # change that to "sub whatever {" to name this function
  no locale; # we need the real 8-bit ASCIIbetical sort()
  use strict;
  return
    # map sort map is the Schwartzian Transform.  See perlfaq4.
    map { \$_->[0] }
    sort {
           \$a->[1] cmp \$b->[1] ||
           do {
             foreach my \$x (\$a, \$b) {
               if( !defined(\$x->[2]) and defined(\$x->[0]) ) {
$minor_code
               }
             }
             \$a->[2] cmp \$b->[2]; # return value of this do-block
           }
         }
    map { [ \$_,
$major_code
            , undef
          ]
        }
    \@_;
}
$coda

EOVOODOO

} else { # one-level mode

  $code = <<"EOVOODOO2";
\# Generated by Sort::ArbBiLex v$VERSION at $now GMT
$prelude
# One-level mode
sub {   # change that to "sub whatever {" to name this function
  no locale; # we need the real 8-bit ASCIIbetical sort()
  use strict;
  return
    # map sort map is the Schwartzian Transform.  See perlfaq4.
    map { \$_->[0] }
    sort { \$a->[1] cmp \$b->[1] }
    map { [ \$_,
$major_code
          ]
        }
    \@_;
}
$coda

EOVOODOO2

}

  print "\nCode to eval:\n", $code, "__ENDCODE__\n\n" if $Debug;

  return $code;
}

###########################################################################
1;

__END__

=head1 NAME

Sort::ArbBiLex -- make sort functions for arbitrary sort orders

=head1 SYNOPSIS

  use Sort::ArbBiLex (
    'fulani_sort',   # ask for a &fulani_sort to be defined
    "a A
     c C
     ch Ch CH
     ch' Ch' CH'
     e E
     l L
     lh Lh LH
     n N
     r R
     s S
     u U
     z Z
    "
  );
  @words = <>;
  @stuff = fulani_sort(@words);
  foreach (@stuff) { print "<$_>\n" }

=head1 CONCEPTS

Writing systems for different languages usually have specific sort
orders for the glyphs (characters, or clusters of characters) that each
writing system uses.  For well-known national languages, these different
sort orders (or someone's idea of them) are formalized in the locale
for each such language, on operating system flavors that support
locales.  However, there are problems with locales; cf. L<perllocale>.
Chief among the problems relevant here are:

* The basic concept of "locale" conflates language/dialect, writing
system, and character set -- and country/region, to a certain extent.
This may be inappropriate for the text you want to sort.  Notably,
this assumes standardization where none may exist (what's THE sort
order for a language that has five different Roman-letter-based
writing systems in use?).

* On many OS flavors, there is no locale support.

* Even on many OS flavors that do suport locales, the user cannot
create his own locales as needed.

* The "scope" of a locale may not be what the user wants -- if you
want, in a single program, to sort the array @foo by one locale, and
an array @bar by another locale, this may prove difficult or
impossible.

In other words, locales (even if available) may not sort the way you
want, and are not portable in any case.

This module is meant to provide an alternative to locale-based
sorting.

This module makes functions for you that implement bi-level
lexicographic sorting according to a sort order you specify.
"Lexicographic sorting" means comparing the letters (or properly,
"glyphs", as I'll call them here, when a single glyph can encompass
several letters, as with digraphs) in strings, starting from the start
of the string (so that "apple" comes after "apoplexy", say) -- as
opposed to, say, sorting by numeric value.  "Lexicographic sorting" is
sometimes used to mean just "ASCIIbetical sorting", but I use it to
mean the sort order used by I<lexicograph>ers, in dictionaries (at
least for alphabetic languages).

Consider the words "resume" and "rE<eacute>sumE<eacute>" (the latter
should display on your POD viewer with acute accents on the e's).  If
you declare a sort order such that e-acute ("E<eacute>") is a letter
after e (no accent), then "rE<eacute>sumE<eacute>" (with accents)
would sort after every word starting with "re" (no accent) -- so
"rE<eacute>sumE<eacute>" (with accents) would come after "reward".

If, however, you treated e (no accent) and e-acute as the same letter,
the ordering of "resume" and "rE<eacute>sumE<eacute>" (with accents)
would be unpredictable, since they would count as the same thing --
whereas "resume" should always come before "rE<eacute>sumE<eacute>"
(with accents) in English dictionaries.

What bi-level lexicographic sorting means is that you can stipulate
that two letters like e (no accent) and e-acute ("E<eacute>")
generally count as the same letter (so that they both sort before
"reward"), but that when there's a tie based on comparison that way
(like the tie between "resume" and "rE<eacute>sumE<eacute>" (with
accents)), the tie is broken by a stipulation that at a second level,
e (no accent) does come before e-acute ("E<eacute>").

(Some systems of sort order description allow for any number of levels
in sort orders -- but I can't imagine a case where this gets you
anything over a two-level sort.)

Moreover, the units of sorting for a writing system may not be
characters exactly.  In some forms of Spanish, ch, while two
characters, counts as one glyph -- a "letter" after c (at the first
level, not just the second, like the e in the paragraph above).  So
"cuerno" comes I<before> "chile".  A character-based sort would not be
able to see that "ch" should count as anything but "c" and "h".  So
this library doesn't assume that the units of comparison are
individual characters.

=head2 Limitations

* The most notable limitation of this module is that its
identification of glyphs must be context-insensitive.  So you can't
stipulate that, for example, ":" normally counts as a letter after
"h", but that it doesn't count (or that it counts as a letter after
"z", or whatever) in the special case of appearing at the start of
words.

* You can't declare whitespace characters of any kind as sortable
glyphs using the single-string ("short form") declaration.  This is,
obviously, because in that declaration format, whitespace is reserved
as the delimiter for glyphs and families.  So if you want to have
space, tab, CR, and/or LF be sortable glyphs, you just have to declare
that with the long form (LoL-reference) format.  See the sections on
these formats, below.

* When you have Sort::ArbBiLex generate a new bi-level sort function
based on a sort-order declaration, both levels of comparison obviously
have the same sort-order declaration -- so you can't have
Sort::ArbBiLex make a function where at one level "ch" counts as one
glyph, and at the other, it counts as two; nor where it counts as a
glyph in one position in one level, and at another position in the
other level.

* When you declare a glyph as consisting of several characters, you're
saying that several letters should be considered as one unit.
However, you can't go the other way: you can't say that a single
letter should be considered as a combination of glyphs.  But I've seen
some descriptions of German sort order that say that a-umlaut (etc)
should be treated as if it were a literal "ae" -- i.e. an "a" glyph
followed by an "e" glyph.  This can't be done simply with
Sort::ArbBiLex.

* Note that ArbBiLex-generated sort routines always start sorting (at
both levels) with glyphs at the start of the string, and continue to
the end.  But some descriptions (like p138 of the I<Unicode Standard
Version 3.0>) of French sort order say that the the first level of
sorting goes as you'd expect, start to finish, but a later level, ties
between different accents are broken I<starting from the end>, and
working backwards.  This can't be done simply with Sort::ArbBiLex.
(But it's my experience that the difference is not significant, in the
case of French data.)

* Currently, you cannot declare more than 255 glyph-groups (i.e.,
glyphs that sort the same at the first level), and no glyph-group can
contain more that 255 glyphs each.  (This may change in the future.)
However, it's fine if the total number of glyphs in all glyph-groups
sums to greater than 255 (as in the case of a declaration for 30
glyph-groups with 10 glyphs each).

* I'm writing this module before all details of Perl's future handling
of text in Unicode are clear.  This may change how this module works,
although I'm hoping it will not introduce any incompatibilities.

* This library makes no provision for overriding the builtin C<sort>
function.  It's probably a bad idea to try to do so, anyway.

* If all of the glyphs in a given sort order are one character long, the
resulting sorter function will be rather fast.  If I<any> of them
are longer than that, it is rather slower.  (This is because
one-character mode does its work with lots of C<tr///>'s, whereas
"multi"-character mode (i.e., if I<any> glyphs are more than one
character long) uses lots of C<s///>'s and hashes.  It's as fast as I
can make it, but it's still necessarily much slower than
single-character mode.  So if you're sorting 10,000 dictionary
headwords, and you change your sort order from one that uses all
one-character glyphs, to one where there's even just one two-character
glyph, and you notice that it now takes 15 seconds instead of 3
before, now you know why.

* I<Remember>, if this module produces a function that I<almost> does
what you want, but doesn't exactly, because of the above limitations,
then you can be have it output the source (via C<source_maker>) of the
function, and try modifying that function on your own.

=head1 DESCRIPTION

This module provides two functions, C<Sort::ArbBiLex::maker> and
C<Sort::ArbBiLex::source_maker>, and it also presents an interface
that accepts parameters in the C<use Sort::ArbBiLex ( ... )>
statement.

=head2 C<use Sort::ArbBiLex;>

This merely loads the module at compile-time, just like any normal
"use I<[modulename]>".  But with parameters, it's special:

=head2 C<use Sort::ArbBiLex ( 'name', DECLARATION, ...  );>

This compile-time directive, besides loading the module if it's not
already in memory, will interpret the parameters as a list of pairs,
where each pair is first the name of a sub to create and then the
DECLARATION of its sort order.  This calls
C<Sort::ArbBiLex::maker(DECLARATION)> to make a such-named function,
that sorts according to the sort order you specify.

This is probably the only way most users will need to interact with
this module; they probably won't need to call
C<Sort::ArbBiLex::maker(DECLARATION)> (much less
C<Sort::ArbBiLex::source_maker(DECLARATION)>!) directly.

Unless your sort-order declarations are variables, you can simply use
this C<use Sort::ArbBiLex (...)> syntax.  Feel free to skip ahead to
the "Values for DECLARATION" section.

=head2 The function Sort::ArbBiLex::maker(DECLARATION)

C<maker> is called thus:

  Sort::ArbBiLex::maker(DECLARATION)

This will make a sort function, based on the contents of DECLARATION.
The return value is an anonymous subroutine reference.  While you can
store this just like any another anonymous subroutine reference, you
probably want to associate it with name, like most functions.  To
associate it with the symbol C<fulani_sort> in the current package,
do this:

  *fulani_sort = Sort::ArbBiLex::maker($my_decl);

Then you can call C<fulani_sort(@whatever)> just like any other kind
of function, just as if you'd defined C<fulani_sort> via:

  sub fulani_sort {
    ...blah blah blah...
  }

As you might expect, you can specify a package, like so:

  *MySorts::fulani_sort = Sort::ArbBiLex::maker($my_decl);

If you don't know what C<*thing = EXPR> means or how it works,
don't worry, just use it -- or duck the whole issue by using the
"C<use Sort::ArbBiLex ('fulani_sort', DECL>".

Actually, there's a minor difference between the various ways of
declaring the subroutine C<fulani_sort>: if you declare it via a call
to this:

  *fulani_sort = Sort::ArbBiLex::maker($my_decl);

then that happens at runtime, not compile time.  However, compile-time
is when Perl wants to know what subs will exist I<if> you want to be
able to call them without parens.  I.e., this:

  @stuff = fulani_sort @whatever;  # no parens!

will cause all sorts of angry error messages, which you can happily
avoid by simply adding a "forward declaration" at some early point in
the program, to express that you're goung to want to use "fulani_sort"
as a sub name:

  sub fulani_sort;  # yup, just that!
  ....later...
  @stuff = fulani_sort @whatever;  # no parens!

And then all should be well.

The short story is to use the "C<use Sort::ArbBiLex ('fulani_sort',
...)>" syntax whenever possible (at which point you're free to omit
parens, since the "use" makes it happen at compile-time, not runtime).

But when you can't use the "C<use Sort::ArbBiLex ('fulani_sort',
...)>" syntax, and you need to use a "C<*foo = ...>" syntax instead
(which I<is> usually necessary if your declaration is a variable,
instead of a literal), then either add a "C<sub fulani_sort;>" line to
your program; or just be sure to use parens on every call to the
C<fulani_sort> function.

=head2 The function Sort::ArbBiLex::source_maker(DECLARATION)

C<Sort::ArbBiLex::source_maker> is just like C<Sort::ArbBiLex::maker>,
except that it returns a string containing the source of the function.
It's here if you want to, well, look at the source of the function, or
write it to a file and modify it.

=head1 Values for DECLARATION

DECLARATION is a specification of the sort order you want the new
function to sort according to.

It can occur in two formats: short form (a single string), and long
form (a reference to a list of lists of glyphs).

=head2 Short-form DECLARATION values

A short-form specification consists of a string that consists of lines
containing glyphs.  The example in the SYNOPSIS section shows this
format.

Formally, lines in the short-form declaration string are separated by
carriage returns and/or linefeeds.  Each line consists of glyphs
separated by whitespace (other than CRs or LFs).  Lines that are empty
(i.e., which contain no glyphs) are ignored.  A declaration that
contains no glyphs at all is illegal, and causes a fatal error.  A
"glyph" is any sequence of non-whitespace characters.  No glyph can
appear more that once in the declaration, or it's a fatal error.

A degenerate case of there being only one glyph-family with many
glyphs in it (i.e., a one-level sort instead of a bi-level), like
this:

  Sort::ArbBiLex::maker('fulani_sort',
    "a A c C c' C' e E h H x X i I : l L n N r R s S u U z Z zh Zh ZH"
  );

is actually treated as if it were that many glyph-families with only
one glyph in each.  This in an internal optimization.

PLEASE NOTE that any characters that I<are> in the data being sorted
but which I<do not> appear in the sort order declaration (neither as
themselves, or as part of glyphs) are treated I<as if they are not
there>.  In other words, given the sort order in the above example, if
you had "David" as an item to sort, it would sort just as if it were
"ai" -- since "D", "v", and "d" aren't in that declaration.  So think
twice before deciding that certain letters "are not part of the
alphabet" of the language in question.

Note also that if, say, "ch" is in the sort order, but "h" isn't, then
an "h" I<not> after a "c" (like in "helicopter" or "pushed") will not
be counted for sake of sorting.

=head2 Long-form DECLARATION values

A long-form specification consists of a reference to a list of lists
of glyphs.  For example, the example from the SYNOPSIS section could
just as well be denoted with a long-form declaration like this:

  use Sort::ArbBiLex ( 'fulani_sort',
   [
     [ "a", "A" ],
     [ "c", "C" ],
     [ "ch", "Ch", "CH" ],
     [ "ch'", "Ch'", "CH'" ],
     [ "e", "E" ],
     [ "l", "L" ],
     [ "lh", "Lh", "LH" ],
     [ "n", "N" ],
     [ "r", "R" ],
     [ "s", "S" ],
     [ "u", "U" ],
     [ "z", "Z" ],
   ]
  );

The main practical reason I provide this declaration is that, as
discussed in the Limitations section, the short form doesn't allow you
to declare whitespace characters of any kind as sortable glyphs using
the single-string ("short form") declaration, because in that
declaration format, whitespace is reserved as the delimiter for glyphs
and families.  But you can do it in the long form.  In the above
example, you'd just add a line before the one for a/A, like this:

  use Sort::ArbBiLex ( 'fulani_sort',
    [
     [ " ", "\t", "\cm", "\cj" ],  # whitespace characters.
     [ "a", "A" ],
    ...etc...

That'd make whitespace the first glyph family.  The effect of this
would be to make sorting sensitive to whitespace, such that "for",
"for sure", and "forest" would sort in that order.  It's I<my>
impression that most modern English dictionaries sort without respect
to whitespace (so that that list, sorted, would be "for", "forest",
"for sure"), but I also realize that that's not the only way to do it.
In fact, sensitivity to whitespace seems an inevitable part of
conventional sort orders for some languages.

A thought: Presumably the only place you'd want to put the whitespace
family is at the start of the declaration.  It'd be really strange in
the middle or the end, I think.

A word of caution: Note that, if you have whitespace as a glyph
family, "for sure" (with just the one space) and "for  sure" (with two
spaces inbetween) do I<not> sort the same.  You may expect that the
sorter would magically collapse whitespace, seeing all sequences of
whitespace as equal.  Au contraire!  They're glyphs, just like any
others, so sorting "for sure" and "for  sure" (two spaces) is
totally analogous to sorting "zabifanl" and "zabiifanl".

=head1 GUTS

* ArbBiLex stands for "arbitrary bi-level lexicographic".

* The source to this module may tie your brain in knots.  That's what it
does to me, I<and I wrote it>.  Code that writes code can be icky that
way.

If you want to figure out this module's workings, try using:

  print Sort::ArbBiLex::source_maker($decl);

where you start out with $decl as something short.  Understand the
code that this module makes before you try to understand how it makes
it.

* The sorter functions this module makes are built around the
Schwartzian transform -- i.e., the construct "map BLOCK sort BLOCK map
BLOCK List".  For a brief discussion of that, see L<perlfaq4>, under
the question "How do I sort an array by (anything)?".  Or maybe I'll
write a I<Perl Journal> (www.tpj.com) article about it some time.

* If you look at, say,

  use Sort::ArbBiLex (fu => "a b c ch d i h s sh x");
  @x = fu( qw(chachi baba) );

and if you wonder how the "ch" in "chachi" ends up as one glyph "ch"
(which is what happens), instead of as a glyph "c" and a glyph "h"
(which is I<not> what happens), then consider that this

  print "<$1>" while 'chache' =~ /(ch|c|h|a|e)/g

prints "<ch><a><ch><e>", not "<c><h><a><c><h><e>".

* While you're at it, consider that this:

  print "<$1>" while "itlha" =~ /(tl|lh|l|h|i|a)/g

prints "<i><tl><h><a>", never "<i><t><lh><a>".  Presumably this is
always The Right Thing.

* Most modules that define an C<import> method (either directly, or by
inheritating from the Exporter module), use it to just export
pre-existing subs from their own package into the calling package.
But Sort::ArbBiLex's C<import> is different -- there's no pre-existing
subs to export, so it just makes new anonymous subs on demand, and
sticks them into the package that's current for the given "C<use
Sort::ArbBiLex>" line (unless the user specifies some other package,
which is uncommon).

=head1 COPYRIGHT

Copyright 1999, 2000, Sean M. Burke C<sburke@cpan.org>, all rights
reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

Sean M. Burke C<sburke@cpan.org>

=cut
