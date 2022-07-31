#! /usr/bin/perl -w

$copyright = <<"EOF";
  This file is part of the FMPFR library.

  Copyright 2022 by Thomas Koenig

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use, copy,
  modify, merge, publish, distribute, sublicense, and/or sell copies
  of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
EOF

# Automatically generate the files for compilation from the MPFR
# documentation.  Feed this script with

# grep ^@deftypefun mfpr.texi | perl generate.pl

# and it will generate the necessary files.

for $line (split ("\n", $copyright))
{
    push (@copyright, "! " . $line);
}

$f_copyright = join("\n", @copyright,"\n");

# Automated generation of Fortran interfaces to mpfr.  Run the
# function definitions from the MPFR documentation through this
# script to generate the bindings.

# This script could do with a thorough rewrite, but it serves its
# purpose, and is only supposed to be run once.

# These types are equivalent to the corresponding Fortran type.

$atype{"int"} = "integer (c_int), value";
$atype{"long int"} = "integer (c_long), value";
$atype{"long"} = "integer (c_long), value";
$atype{"long *"} = "integer (c_long), intent(out)";
$atype{"char *"} = "type (c_ptr), value";
$atype{"char **"} = "type (c_ptr)";
$atype{"const char *"} = "type (c_ptr), value";
$atype{"size_t"} = "integer (c_size_t), value";
$atype{"mpfr_rnd_t"} = "integer (c_int), value";
$atype{"float"} = "real (c_float), value";
$atype{"double"} = "real (c_double), value";
$atype{"long double"} = "real (c_long_double), value";
$atype{"mpfr_t"} = "type (mpfr_t)";
$atype{"_Float128"} = "real (qp), value";
$atype{"mpfr_prec_t"} = "integer (mpfr_prec_kind), value";
$atype{"mpfr_exp_t"} = "integer (mpfr_exp_kind), value";
$atype{"mpfr_exp_t *"} = "integer (mpfr_exp_kind)";
$atype{"int *"} = "integer (c_int)";
$atype{"long*"} = "integer (c_long)";
$atype{"void *"} = "type (c_ptr), value";
$atype{"mpfr_flags_t"} = "integer (c_int), value";
$atype{"mpfr_free_cache_t"} = "integer (c_int), value";
$atype{"intmax_t"} = "integer (c_intmax_t), value";
$atype{"short"} = "integer (c_short), value";
$atype{"char"} = "integer (c_char), value";

# Unsupported types in Fortran.

@unsupported = ("mpz_t", "mpq_t", "mpf_t", "unsigned long int", "uintmax_t",
		"intmax_t", "const mpfr_ptr", "unsigned long", "FILE *",
		"gmp_randstate_t"
    );

foreach $k (@unsupported) {
    $unsupported{$k} = 1;
}

$funtype{"si"} = "integer (c_long)";
$funtype{"d"} = "real (c_double)";

foreach $k (keys(%atype)) {
    $v = $atype{$k};
    $v =~ s/,.*//;
    $ftype{$k} = $v;
    $ctype{$k} = $v;
}

$ftype {"mpfr_t"} = "type (fmpfr)";
$ftype {"mpfr_rnd_t"} = "integer (kind=int8)";

@pure = qw(mpfr_get_prec mpfr_init2 mpfr_set_d mpfr_set_flt mpfr_set_si
    mpfr_set mpfr_set_str mpfr_get_si mpfr_get_d mpfr_get_flt mpfr_clear
    mpfr_set_prec mpfr_get_float128 mpfr_get_ld mpfr_set_float128 mpfr_set_ld);

foreach $k (@pure) {
    $pure{$k} = 1;
}

@glue = qw (mpfr_set_str);
foreach $k (@glue) {
    $glue{$k} = 1;
}

@oper = qw(add sub mul div pow);
$opinter{"add"} = '+';
$opinter{"sub"} = '-';
$opinter{"mul"} = '*';
$opinter{"div"} = '/';
$opinter{"pow"} = '**';

$funint = [];
$opint = [];
@assint = ();
push (@assint, "    module procedure ass_set\n");

@public = ();

for $k (qw(add mul)) {
    $reverse{$k} = 1;
}

for $k (qw(sin cos tan asin acos atan sqrt atan2 exp log log10 gamma abs min max)) {
    $intrin{$k} = $k;
}

for $k (qw(si d flt ld float128)) {
    $settype{$k} = 1;
}

$compare{"greater"} = ">";
$compare{"greaterequal"} = ">=";
$compare{"less"} = "<";
$compare{"lessequal"} = "<=";
$compare{"lessgreater"} = "/=";
$compare{"equal"} = "==";

for $k (keys(%compare)) {
    $compare_fun{"mpfr_" . $k . "_p"} = 1;
}
$olines = "";

open KIND, ">fmpfr_kinds.f90.in" or die "Cannot open fmpfr_kinds.f90.in: $!";
print KIND <<"EOF";
module fmpfr_kinds
  use iso_c_binding
  implicit none
  integer, parameter, public :: mpfr_prec_kind = \@mpfr_prec_t\@
  integer, parameter, public :: mpfr_exp_kind = \@mpfr_exp_t\@
    type, bind(C) :: mpfr_t
     private
     integer (kind=mpfr_prec_kind), public :: mpfr_prec
     integer (kind=c_int) :: mpfr_sign
     integer (kind=mpfr_exp_kind) :: mpfr_exp
     type (c_ptr) :: mpfr_d
  end type mpfr_t
  integer(kind=mpfr_prec_kind) :: default_prec = \@default_precision\@
  integer, parameter :: qp = selected_real_kind(33,4000)
end module fmpfr_kinds
EOF
    
open CBIND, ">fmpfr_c.F90" or die "Kann fmpfr_c.F90 nicht oeffnen: $!";
print CBIND $f_copyright;

print CBIND <<"EOF";
module fmpfr_c
  use fmpfr_kinds
  interface
EOF
open OPER, ">fmpfr_oper.F90" or die "Kann fmpfr_oper.F90 nicht oeffnen: $!";
print OPER $f_copyright;
print OPER <<"EOF";
module fmpfr_oper
  use fmpfr_c
  use iso_fortran_env
  implicit none
  private
  type fmpfr
    type (mpfr_t) :: mp
    logical :: initialized = .false.
  contains
    final :: fmpfr_cleanup
  end type fmpfr

  integer (kind=int8), parameter :: mpfr_rndn = 0, mpfr_rndz = 1, &
    mpfr_rndu = 2, mpfr_rndd = 3, mpfr_rnda = 4, mpfr_rndf = 5, &
    mpfr_rndna = -1

  integer (c_int) :: default_rnd

  interface
     function strlen (s) bind(c) result(res)
       import
       implicit none
       integer (c_size_t) :: res
       type (c_ptr), value :: s
     end function strlen
  end interface
EOF

for $x (qw(mpfr_rndn mpfr_rndz mpfr_rndu mpfr_rndd mpfr_rnda
	mpfr_rndf mpfr_rndna)) {
    push (@public, "  public :: $x\n");
}
      
open CGLUE, ">fmpfr_glue.c" or die "Kann fmpfr_glue.c nicht oeffnen: $!";
print CGLUE "/*\n", $copyright,"*/\n\n";
print CGLUE <<"EOF";
#if USE_FLOAT128
#define MPFR_WANT_FLOAT128
#endif
#include <mpfr.h>
EOF

$fglue = "\n  interface\n";

LINE: while (<>) {
    next if /\@dots/ or /\.\.\./ or /va_list/;
    next if /decimal/;
    s/^[@]deftypefunx?\s+//;
    ($type, $name, $arglist) = m/^(\w+)\s+(\w+)\s+\((.*)\)/;
    if (!defined($type)) {
	($type, $name, $arglist) = m/^\{(.*)\}\s+(\w+)\s+\((.*)\)/;
    }

    $subroutine = $type eq "void";
    next LINE unless defined($ftype{$type}) or $subroutine;
    $xname = ($subroutine ? "subroutine " : "function ") . $name;
    @vars = ();
    @argtypes = ();
    if ($arglist ne "void") {
	@args = split(/, */,$arglist);
	foreach $arg (@args[0..$#args]) {
	    ($t, $v) = ($arg =~ m/^(.*)\@var\{(\w+)\}/);
	    $t =~ s/ +$//;
	    if (!defined($t)) {
		print STDERR $arglist,"\n";
		print STDERR "arg = '",$arg,"'\n";
		print STDERR $_;
		exit 1;
	    }
	    next LINE if defined($unsupported{$t});
	    if (!defined($atype{$t})) {
		print STDERR "-->$t<--\n";
		exit 1;
	    }
	    push (@argtypes, $t);
	    push (@vars, $v);
	}
    }
    $is_float128 = grep (/^_Float128/, @argtypes) || $type =~ m/_Float128/;
    $is_long_double = grep (/^long double/, @argtypes) || $type =~ m/long double/;
    $pure = exists($pure{$name}) || exists($compare_fun{$name}) ? "pure " : "";
    print CBIND "#if USE_FLOAT128\n" if $is_float128;
    print CBIND "#if USE_LONG_DOUBLE\n" if $is_long_double;
    print CBIND "    $pure", $xname, " (", join(", ",@vars), ")";
    print CBIND " result (ret)" unless $subroutine;
    print CBIND " bind(c)\n      import\n      implicit none\n";
    for $i (0..$#argtypes) {
	if ($pure && $atype{$argtypes[$i]} !~ /intent/) {
	    $intent = ", intent(in)";
	}
	else {
	    $intent = "";
	}
	print CBIND "      ",$atype{$argtypes[$i]},"$intent :: ", $vars[$i],"\n";
    }
    print CBIND "      ",$ctype{$type}, " :: ret\n" unless $subroutine;
    print CBIND "    end ", $xname, "\n";
    print CBIND "#endif\n\n" if $is_float128 || $is_long_double;

    &write_glue if (exists($glue{$name}));
    (undef, $part1, $part2, $part3) = split("_", $name);
    if (exists($compare{$part1})) {
	&handle_compare ($part1);
	next;
    }
  OP:
    foreach $op (@oper) {
	next OP unless $part1 eq $op || (defined($part2) && $part2 eq $op);
	next OP if defined($part2) && $part2 =~ m/2/;
	&handle_stuff;
  }
  INTRIN:
    foreach $op (keys %intrin) {
	next INTRIN unless $part1 eq $op && !defined($part2);
	&handle_intrinsic;
  }
    if ($part1 eq "set" && (!defined($part2) || $settype{$part2})
	&& !defined($part3))
  {
      &handle_rop_assign;
  } elsif ($part1 eq "get" && !defined($part3) && defined($part2) && $settype{$part2}) {
      &handle_t_assign;
  }
}
print CBIND "  end interface\n";
print CBIND "end module fmpfr_c\n";
close CBIND;

&misc_routines;

print OPER $fglue,"  end interface\n";

for $f (sort keys(%funint)) {
    print OPER "  public :: $f\n";
    print OPER "  interface $f\n";
    print OPER @{$funint{$f}};
    print OPER "  end interface $f\n\n";
}

for $f (sort keys(%opint)) {
    $x = $opinter{$f};
    print OPER "  public :: operator (",$x,")\n";
    print OPER "  interface operator (",$x,")\n";
    print OPER @{$opint{$f}};
    print OPER "  end interface operator (",$x,")\n\n";
}

for $k (sort keys(%compare_int)) {
    print OPER <<"EOF";
  public :: operator ($k)
  interface operator ($k)
EOF
    print OPER @{$compare_int{$k}};
    print OPER "  end interface\n\n";
}

print OPER "  public :: assignment(=)\n";
print OPER "  interface assignment(=)\n", @assint,
    "end interface assignment(=)\n\n";

print OPER <<"EOF";
#if USE_DERIVED_IO
  public :: write(formatted)
  interface write(formatted)
    module procedure write_formatted
  end interface write(formatted)
#endif
EOF

print OPER @public;
print OPER "contains\n";
print OPER $olines;
print OPER "end module fmpfr_oper\n";
close OPER;

sub handle_stuff
{
    my($stem) = $part1 . (defined($part2) ? "_$part2" : "");
    my (@mpfr_ops, @prec_ops, $i_stuff, $i_rnd);

    &write_glue;
    $olines .= "  elemental function fun_$stem (" . join (", ",@vars[1..$#vars]) .
	") result (" . $vars[0] . ")\n";
    for $i (1..$#vars) {
	$olines .=  "      " . $ftype{$argtypes[$i]} . ", intent(in)";
	if ($argtypes[$i] eq "mpfr_rnd_t") {
	    $i_rnd = $i;
	    $olines .= ", optional ";
	}
	elsif ($argtypes[$i] eq "mpfr_t") {
	    push (@mpfr_ops, $i);
	}
	elsif (exists ($ftype{$argtypes[$i]})) {
	    $i_stuff = $i;
	}
	$olines .=  " :: " . $vars[$i] . "\n";
    }
    @prec_ops = map { "prec_" . $_ } @vars[@mpfr_ops];
    $olines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    $olines .= "      integer (mpfr_prec_kind) :: ";
    $olines .= join (", ", @prec_ops);
    $olines .= ", prec" if $#prec_ops > 0;
    $olines .= "\n";
    $olines .= <<"EOF";
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present($vars[$i_rnd])) rnd_val = $vars[$i_rnd]
      if (.not. $vars[0]%initialized) then
EOF
    for my $val (@mpfr_ops) {
	$olines .= "        prec_$vars[$val] = mpfr_get_prec ($vars[$val]%mp)\n";
    }
    $olines .= "        prec = max (" . join(", ",@prec_ops) . ")\n" if ($#mpfr_ops > 0);
    $olines .= "        call mpfr_init2 (" . $vars[0] . "%mp, ";
    if ($#mpfr_ops > 0) {
	$olines .= "prec)\n";
    } else {
	$olines .= "$prec_ops[0])\n";
    }
    $olines .= "      end if\n";
    $olines .= "      call f$name (";
    my (@alist);
    for $i (0..$#argtypes) {
	if ($argtypes[$i] eq "mpfr_t") {
	    push (@alist,$vars[$i] . "%mp");
	} elsif ($argtypes[$i] eq "mpfr_rnd_t") {
	    push (@alist,"rnd_val");
	} else {
	    push(@alist,$vars[$i]);
	}
    }
    $olines .= join (", ", @alist) . ")\n";
    $olines .= "    end function fun_$stem\n\n";
    push (@{$funint{$op}}, "    module procedure fun_$stem\n");

    return unless $argtypes[$#argtypes] eq "mpfr_rnd_t";
    return unless $opinter{$op};
    $olines .= "    elemental function op_$stem (" .  join(", ",  @vars[1..$#vars-1]) .
	") result (" . $vars[0] . ")\n";
    $fbody = "";
    for $i (1..$#vars-1) {
	$fbody .= "      ". $ftype{$argtypes[$i]} . ", intent(in) :: ". $vars[$i] . "\n";
    }
    $fbody .= "      " . $ftype{$argtypes[0]} . " :: " . $vars[0] . "\n\n";
    $fbody .= "      $vars[0] = fun_$stem (" . join(", " , @vars[1..$#vars-1]) . ")\n";
    $olines .= $fbody;
    $olines .= "    end function op_$stem\n\n";
    push (@{$opint{$op}}, "    module procedure op_$stem\n");
    return unless defined $part2;
    if ($part1 eq "si" or $part2 eq "si") {
	$olines .= "#if SIZEOF_INT < SIZEOF_LONG\n";
	&conv_type ("long int", "int", $stem);
	$olines .= "#endif\n\n";
	&conv_type ("long int", "short", $stem);
    }
    elsif ($part1 eq "d" or $part2 eq "d") {
	&conv_type ("double", "float", $stem);
    }
    return unless exists($reverse{$op});
    $stem2 = $part2 . "_$part1";
    $olines .= "    elemental function op_$stem2 (" .
	join(", ", $vars[2], $vars[1], @vars[3..$#vars-1]) .
	") result (" . $vars[0] . ")\n";
    $olines .= $fbody;
    $olines .= "    end function op_$stem2\n\n";    
    push (@{$opint{$op}}, "    module procedure op_$stem2\n");
    if ($part2 eq "si") {
	$olines .= "#if SIZEOF_INT < SIZEOF_LONG\n";
	&conv_type_rev ("long int", "int", $stem, $stem2);
	$olines .= "#endif\n\n";
	&conv_type_rev ("long int", "short", $stem, $stem2);
    } elsif ($part2 eq "d") {
	&conv_type_rev ("double", "float", $stem, $stem2);
    }
}

# Handle intrinsic functions like sin, cos, sqrt, ...

sub handle_intrinsic
{
    @mpfr_ops = ();
    &write_glue;
    $olines .= "  elemental function fun_$op (" . join(", ", @vars[1..$#vars]) .
	") result (" . $vars[0] . ")\n";
    for $i (1..$#vars) {
	$olines .=  "      " . $ftype{$argtypes[$i]} . ", intent(in)";
	if ($argtypes[$i] eq "mpfr_rnd_t") {
	    $i_rnd = $i;
	    $olines .= ", optional ";
	} elsif ($argtypes[$i] eq "mpfr_t") {
	    push (@mpfr_ops, $i);
	}
	$olines .=  " :: " . $vars[$i] . "\n";
    }
    @prec_ops = map { "prec_" . $_ } @vars[@mpfr_ops];
    $olines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    $olines .= "      integer (mpfr_prec_kind) :: ";
    $olines .= join (", ", @prec_ops);
    $olines .= ", prec" if $#prec_ops > 0;
    $olines .= "\n";
    $olines .= <<"EOF";
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present($vars[$i_rnd])) rnd_val = $vars[$i_rnd]
      if (.not. $vars[0]%initialized) then
EOF
    for my $val (@mpfr_ops) {
	$olines .= "        prec_$vars[$val] = mpfr_get_prec ($vars[$val]%mp)\n";
    }
    $olines .= "        prec = max (" . join(", ",@prec_ops) . ")\n" if ($#mpfr_ops > 0);
    $olines .= "        call mpfr_init2 (" . $vars[0] . "%mp, ";
    if ($#mpfr_ops > 0) {
	$olines .= "prec)\n";
    } else {
	$olines .= "$prec_ops[0])\n";
    }
    $olines .= "      end if\n";
    $olines .= "      call f$name (";
    my (@alist);
    for $i (0..$#argtypes) {
	if ($argtypes[$i] eq "mpfr_t") {
	    push (@alist,$vars[$i] . "%mp");
	} elsif ($argtypes[$i] eq "mpfr_rnd_t") {
	    push (@alist,"rnd_val");
	} else {
	    push(@alist,$vars[$i]);
	}
    }
    $olines .= join (", ", @alist) . ")\n";
    $olines .= "  end function fun_$op\n\n";
    push (@{$funint{$op}}, "    module procedure fun_$part1\n");
}

# For functions which have "long int" arguments, for example, we want
# to offer the programmer "int" and "short" for convenience.

sub conv_type
{
    my ($from, $to, $stem) = @_;
    my (@at, $ip, $tmp_var, @va);
    $ip = -1;
    for my $i (1..$#argtypes) {
	if ($argtypes[$i] eq $from) {
	    $ip = $i;
	    $at[$i] = $to;
	    $tmp_var = $vars[$i] . "_tmp";
	    push(@va, $tmp_var);
	}
	else {
	    $at[$i] = $argtypes[$i];
	    push(@va, $vars[$i]);
	}
    }
    my ($xname) = "fun_$stem" . "_$to";
    $olines .= "  elemental function $xname (" . join(", ", @vars[1..$#vars]) .
	") result (" . $vars[0] . ")\n";
    for $i (1..$#vars) {
	$olines .=  "      " . $ftype{$at[$i]} . ", intent(in)";
	if ($at[$i] eq "mpfr_rnd_t") {
	    $olines .= ", optional ";
	}
	$olines .=  " :: " . $vars[$i] . "\n";
    }
    $olines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    $olines .= "      $ftype{$from} :: $tmp_var\n";
    $olines .= "      $tmp_var = $vars[$ip]\n";
    $olines .= "      $vars[0] = fun_$stem (";
    $olines .= join (", ",@va) . ")\n";
    $olines .= "  end function $xname\n\n";
    $yname = "op_$stem" . "_$to";
    $olines .= "  elemental function $yname (" . join(", ", @vars[1..$#vars-1]) .
	") result (" . $vars[0] . ")\n";
    for $i (1..$#vars - 1) {
	$olines .=  "      " . $ftype{$at[$i]} . ", intent(in)" .
	    " :: " . $vars[$i] . "\n";
    }
    $olines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    $olines .= "      $vars[0] = $xname (" . join(", ", @vars[1..$#vars-1]) . ")\n";
    $olines .= "  end function $yname\n\n";
    
    if ($to eq "int") {
	push (@{$funint{$op}},"#if SIZEOF_INT < SIZEOF_LONG\n");
	push (@{$funint{$op}},"    module procedure $xname\n");
	push (@{$funint{$op}},"#endif\n");
	push (@{$opint{$op}},"#if SIZEOF_INT < SIZEOF_LONG\n");
	push (@{$opint{$op}},"    module procedure $yname\n");
	push (@{$opint{$op}},"#endif\n");

    } else {
	push (@{$funint{$op}},"    module procedure $xname\n");
	push (@{$opint{$op}},"    module procedure $yname\n");
    }
}

# Operators for a + b should also offer b + a.

sub conv_type_rev
{
    my ($from, $to, $stem, $stem2) = @_;
    my ($yname, $xname);
    my (@at, $ip, $tmp_var, @va);
    $yname = "op_$stem2" . "_$to";
    $xname = "fun_$stem";
    $olines .= "    elemental function $yname (" .
	join(", ", $vars[2], $vars[1], @vars[3..$#vars-1]) .
	") result (" . $vars[0] . ")\n";
    for my $i (1..$#argtypes-1) {
	if ($argtypes[$i] eq $from) {
	    $ip = $i;
	    $at[$i] = $to;
	    $tmp_var = $vars[$i] . "_tmp";
	    push(@va, $tmp_var);
	}
	else {
	    $at[$i] = $argtypes[$i];
	    push(@va, $vars[$i]);
	}
    }
    for $i (1..$#vars-1) {
	$olines .=  "      " . $ftype{$at[$i]} . ", intent(in)";
	if ($at[$i] eq "mpfr_rnd_t") {
	    $olines .= ", optional ";
	}
	$olines .=  " :: " . $vars[$i] . "\n";
    }
    $olines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    $olines .= "      $ftype{$from} :: $tmp_var\n";
    $olines .= "      $tmp_var = $vars[$ip]\n";
    $olines .= "      $vars[0] =  $xname (" . join(", ", @va[0..$#vars-2]) . ")\n";
    $olines .= "    end function $yname\n\n";
    if ($to eq "int") {
	push (@{$opint{$op}},"#if SIZEOF_INT < SIZEOF_LONG\n");
	push (@{$opint{$op}},"    module procedure $yname\n");
	push (@{$opint{$op}},"#endif\n");

    } else {
	push (@{$opint{$op}},"    module procedure $yname\n");
    }
}

# Handle assignment.

sub handle_rop_assign
{
    my($stem) = $part1 . (defined($part2) ? "_$part2" : "");
    my($nlines) = "";
    &write_glue;
    $nlines .= "#if USE_FLOAT128\n" if $is_float128;
    $nlines .= "#if USE_LONG_DOUBLE\n" if $is_long_double;

    $nlines .= "    elemental function fun_$stem (" . join (", ",@vars[1..$#vars]) .
	") result (" . $vars[0] . ")\n";
    for $i (1..$#vars) {
	$nlines .=  "      " . $ftype{$argtypes[$i]} . ", intent(in)";
	if ($argtypes[$i] eq "mpfr_rnd_t") {
	    $i_rnd = $i;
	    $nlines .= ", optional";
	}
	elsif ($argtypes[$i] eq "mpfr_t") {
	    push (@mpfr_ops, $i);
	}
	elsif (exists ($ftype{$argtypes[$i]})) {
	    $i_stuff = $i;
	}
	$nlines .=  " :: " . $vars[$i] . "\n";
    }
    @prec_ops = map { "prec_" . $_ } @vars[@mpfr_ops];
    $nlines .= "      " . $ftype{$argtypes[0]} . " :: ". $vars[0] . "\n";
    if ($#prec_ops > 0) {
	$nlines .= "      integer (mpfr_prec_kind) :: ";
	$nlines .= join (", ", @prec_ops) . "\n";
    }
    $nlines .= "\n";

    $nlines .= <<"EOF";
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present($vars[$i_rnd])) rnd_val = $vars[$i_rnd]
      if (.not. $vars[0]%initialized) then
        call mpfr_init2 ($vars[0]%mp, default_prec)
        $vars[0]%initialized = .true.
      end if
EOF
    $nlines .= "      call f$name (";
    my (@alist);
    for $i (0..$#argtypes) {
	if ($argtypes[$i] eq "mpfr_t") {
	    push (@alist,$vars[$i] . "%mp");
	} elsif ($argtypes[$i] eq "mpfr_rnd_t") {
	    push (@alist,"rnd_val");
	} else {
	    push(@alist,$vars[$i]);
	}
    }
    $nlines .= join (", ", @alist) . ")\n";
    $nlines .= "    end function fun_$stem\n\n";

    my($yname);
    $yname = "ass_$stem";

    if (!defined($part2)) {
	$olines .= $nlines;
	return;
    }
    $nlines .= "    elemental subroutine ass_$stem (" .  join(", ",  @vars[0..$#vars-1]) .
	")\n";

    $nlines .= "      " . $ftype{$argtypes[0]} . ", intent(inout) :: " . $vars[0] . "\n";
    for $i (1..$#vars-1) {
	$nlines .= "      ". $ftype{$argtypes[$i]} . ", intent(in) :: ". $vars[$i] . "\n";
    }
    $nlines .= <<"EOF";

      if (.not. rop%initialized) then
        call mpfr_init2 ($vars[0]%mp, default_prec)
        $vars[0]%initialized = .true.
      end if
EOF
    $nlines .= "      call f$name (" . join(", ",@alist[0..$#alist-1],"default_rnd") . ")\n";
    $nlines .= "    end subroutine ass_$stem\n\n";
    $nlines .= "#endif\n" if $is_float128 || $is_long_double;
    # print $nlines;
    $olines .= $nlines;
    if ($is_float128) {
	push (@{$funint{"fmpfr"}}, "#if USE_FLOAT128\n");
	push (@assint, "#if USE_FLOAT128\n");
    } elsif ($is_long_double) {
	push (@{$funint{"fmpfr"}}, "#if USE_LONG_DOUBLE\n");
	push (@assint, "#if USE_LONG_DOUBLE\n");
    }	
    push (@assint, "    module procedure ass_$stem\n");
    push (@{$funint{"fmpfr"}}, "    module procedure fun_$stem\n");
    if ($is_float128 || $is_long_double) {
	push (@{$funint{"fmpfr"}}, "#endif\n");
	push (@assint, "#endif\n");
    }
    if (defined($part2) && $part2 eq "si") {
	$olines .= "#if SIZEOF_INT < SIZEOF_LONG\n";
	push (@assint, "#if SIZEOF_INT < SIZEOF_LONG\n");
	push (@{$funint{"fmpfr"}},"#if SIZEOF_INT < SIZEOF_LONG\n");
	&conv_type_ass ("long int", "int", $stem);
	push (@assint, "#endif\n");
	push (@{$funint{"fmpfr"}},"#endif\n");
	$olines .= "#endif\n";
	&conv_type_ass ("long int", "short", $stem);
    }
}

# This converts ass_set_si into ass_set_si_int and ass_set_si_short.
# Quite specialized, so we can use an almost direct template.

sub conv_type_ass
{
    my($from, $to, $stem) = @_;
    my ($nlines, $xname, $yname);

    $xname = "ass_$stem" . "_$to";
    $nlines = "  elemental subroutine $xname (rop, op)\n";
    $nlines .= <<"EOF";
    type (fmpfr), intent(inout) :: rop
    $ftype{$to}, intent(in) :: op
    $ftype{$from} :: tmp_op;

    tmp_op = op;
    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, default_prec)
      rop%initialized = .true.
    end if
    call f$name (rop%mp, tmp_op, default_rnd)
  end subroutine $xname
EOF

    $yname = "fun_$stem" . "_$to";
    $nlines .= <<"EOF";
  elemental function $yname (op, rnd) result (rop)
    $ftype{$to}, intent(in) :: op
    integer (kind=int8), optional, intent(in) :: rnd
    type (fmpfr) :: rop
    $ftype{$from} :: tmp_op
    integer (c_int) :: rnd_val

    rnd_val = default_rnd
    if (present(rnd)) rnd_val = rnd
    tmp_op = op;
    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, default_prec)
      rop%initialized = .true.
    end if
    call f$name (rop%mp, tmp_op, rnd_val)
  end function $yname
EOF

    $olines .= $nlines;
    push (@assint, "    module procedure $xname\n");
    push (@{$funint{"fmpfr"}}, "  module procedure $yname\n");
}

sub handle_t_assign
{
    my($nlines) = "";
    my($stem) = $part1 . (defined($part2) ? "_$part2" : "");
    my ($i_rnd);

    $nlines .= "#if USE_FLOAT128\n" if $is_float128;
    $nlines .= "#if USE_LONG_DOUBLE\n" if $is_long_double;
    $nlines .= "    elemental function $stem (" . join (", ",@vars[0..$#vars]) .
	") result (rval)\n";
    for $i (0..$#vars) {
	$nlines .=  "      " . $ftype{$argtypes[$i]} . ", intent(in)";
	if ($argtypes[$i] eq "mpfr_rnd_t") {
	    $i_rnd = $i;
	    $nlines .= ", optional";
	}
	elsif ($argtypes[$i] eq "mpfr_t") {
	    push (@mpfr_ops, $i);
	}
	elsif (exists ($ftype{$argtypes[$i]})) {
	    $i_stuff = $i;
	}
	$nlines .=  " :: " . $vars[$i] . "\n";
    }
    $nlines .= "      $ftype{$type} :: rval\n";
    $nlines .= <<"EOF";
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present($vars[$i_rnd])) rnd_val = $vars[$i_rnd]
EOF
    my (@alist);
    for $i (0..$#argtypes) {
	if ($argtypes[$i] eq "mpfr_t") {
	    push (@alist,$vars[$i] . "%mp");
	} elsif ($argtypes[$i] eq "mpfr_rnd_t") {
	    push (@alist,"rnd_val");
	} else {
	    push(@alist,$vars[$i]);
	}
    }
    $nlines .= "      rval = $name (" . join (", ", @alist) . ")\n";
    $nlines .= "    end function $stem\n\n";
    $olines .= $nlines;
    my($plines) = "";
    $plines .= "#if USE_FLOAT128\n" if $is_float128;
    $plines .= "#if USE_LONG_DOUBLE\n" if $is_long_double;
    $plines .= "  public :: $stem\n";
    $plines .= "#endif\n" if $is_float128 || $is_long_double;
    push(@public, $plines);
    $nlines = "  elemental subroutine ass_$stem (" .
	join (", ", "rval", @vars[0..$#vars-1]) . ")\n";
    $nlines .= "    $ftype{$type}, intent(out) :: rval\n";
    for $i (0..$#vars-1) {
	$nlines .= "    " . $ftype{$argtypes[$i]} . ", intent(in) :: "
	    . $vars[$i] . "\n";
    }
    $nlines .= "\n    rval = $stem (" . join(", ", @vars[0..$#vars-1]) . ")\n";
    $nlines .= "  end subroutine ass_$stem\n\n";
    $nlines .= "#endif\n" if $is_float128 || $is_long_double;
    $olines .= $nlines;
    push (@assint, "#if USE_FLOAT128\n") if $is_float128;
    push (@assint, "#if USE_LONG_DOUBLE\n") if $is_long_double;
    push (@assint, "    module procedure ass_$stem\n");
    push (@assint, "#endif\n") if $is_float128 || $is_long_double;
}


# Some miscellaneous routines which do not really lend themselves
# to automated generation from the MPFR function headers.

sub misc_routines
{
    $olines .= <<"EOF";
  function fun_set_str (s, rnd) result(rop)
    type (fmpfr) :: rop
    integer (kind=int8), optional :: rnd
    character(kind=c_char,len=*), intent(in) :: s
    character(kind=c_char), dimension(:), allocatable, target :: s_arg
    integer (kind=c_int) :: rnd_val

    rnd_val = default_rnd
    if (present(rnd)) rnd_val = rnd
    if (.not. rop%initialized) then
       call mpfr_init2 (rop%mp, default_prec)
       rop%initialized = .true.
     end if

    allocate (s_arg(len(s)+1))
    s_arg(1:len(s)) = transfer(s,s_arg)
    s_arg(len(s)+1) = char(0)
    call fmpfr_set_str (rop%mp, c_loc(s_arg), 10, rnd_val)
    deallocate (s_arg)
  end function fun_set_str

  function get_str_work (op, n, rnd) result(res)
    type (fmpfr), intent(in) :: op
    integer (kind=c_size_t), intent(in), optional :: n
    integer (kind=int8), intent(in), optional :: rnd
    character (len=:), allocatable :: res
    character, pointer :: p(:)
    type (c_ptr) :: pc
    integer (c_size_t) :: n_val
    integer (c_int) :: rnd_val
    integer (mpfr_exp_kind) :: exp_val
    integer, parameter :: exp_dig = digits(exp_val)
    character (len=exp_dig+1) :: exp_buffer
    integer (c_size_t) :: slen, exp_len, total_len, i, start

    if (mpfr_nan_p (op%mp) /= 0) then
      res = "NaN"
      return
   else if (mpfr_inf_p(op%mp) /= 0) then
     if (mpfr_signbit(op%mp) == 0) then
       res = "+Inf"
     else
       res = "-Inf"
     end if
     return
    end if
    n_val = 0
    if (present(n)) n_val = n
    rnd_val = default_rnd
    if (present(rnd)) rnd_val = rnd

    pc = mpfr_get_str (c_null_ptr, exp_val, 10, n_val, op%mp, rnd_val)
    slen = strlen(pc)
    call c_f_pointer (pc, p, shape=[slen])
    write (exp_buffer,'(SP,I0)') exp_val-1
    exp_len = len_trim(exp_buffer)
    total_len = slen +  exp_len + 2
    allocate (character(len=total_len) :: res)

    if (p(1) == "+" .or. p(1) == "-") then
      res(1:1) = p(1)
      res(2:2) = p(2)
      res(3:3) = "."
      start = 4
    else
      res(1:1) = p(1)
      res(2:2) = "."
      start = 3
    end if

    do i=start, slen+1
      res(i:i) = p(i-1)
    end do
    res(i:i) = 'E'
    res(i+1:) = exp_buffer(:exp_len)
    call mpfr_free_str (pc)
  end function get_str_work

  function get_str_rnd (op, rnd) result(res)
    type (fmpfr), intent(in) :: op
    integer (kind=int8), intent(in), optional :: rnd
    character (len=:), allocatable :: res
    res = get_str_work (op, 0_c_size_t, rnd)
  end function get_str_rnd

#if USE_DERIVED_IO
  subroutine write_formatted (dtv, unit, iotype, vlist, iostat, iomsg)
    class (fmpfr), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character, pointer :: p(:)
    type (c_ptr) :: pc
    integer (mpfr_exp_kind) :: exp_val
    integer (c_size_t) :: slen, start
  
    if (mpfr_nan_p(dtv%mp) /= 0) then
       write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "NaN"
       return
    else if (mpfr_inf_p(dtv%mp) /= 0) then
       if (mpfr_signbit(dtv%mp) == 0) then
          write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "+Inf"
       else
          write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "-Inf"
       end if
       return
    end if

    pc = mpfr_get_str (c_null_ptr, exp_val, 10, 0_c_size_t, dtv%mp, default_rnd)
    slen = strlen(pc)
    call c_f_pointer (pc,p,shape=[slen])
    if (p(1) == '-' .or. p(1) == '+') then
       write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) p(1)
       start = 2
    else
       start = 1
    end if
    write (unit,fmt='(*(A))',advance="no",iostat=iostat,iomsg=iomsg) p(start)
    if (iostat /= 0) return
    write (unit,fmt='(A)', advance="no", iostat=iostat, iomsg=iomsg) '.'
    if (iostat /= 0) return
    write (unit,fmt='(*(A))',advance="no",iostat=iostat,iomsg=iomsg) p(start+1:),"E"
    if (iostat /= 0) return
    write (unit,fmt='(I0,1X)',iostat=iostat,iomsg=iomsg) exp_val-1
    call mpfr_free_str(pc)
  end subroutine write_formatted

#endif

  elemental subroutine ass_set (rop, op)
    type (fmpfr), intent(inout) :: rop
    type (fmpfr), intent(in) :: op

    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, max(default_prec, op%mp%mpfr_prec))
      rop%initialized = .true.
    end if
    call fmpfr_set (rop%mp, op%mp, default_rnd)    
  end subroutine ass_set

  elemental subroutine fmpfr_cleanup (self)
    type (fmpfr), intent(inout) :: self
    if (self%initialized) call mpfr_clear(self%mp)
    self%initialized = .false.
  end subroutine fmpfr_cleanup

  elemental subroutine init_default (op)
    type (fmpfr), intent(inout) :: op
    if (op%initialized) then
      call mpfr_set_prec (op%mp, default_prec)
    else
      call mpfr_init2 (op%mp, default_prec)
    end if
  end subroutine init_default

  subroutine set_default_rounding_mode (op)
    integer (c_int), intent(in) :: op
    default_rnd = op
    call mpfr_set_default_rounding_mode (op)
  end subroutine set_default_rounding_mode
  
EOF

    push (@public, "  public :: fmpfr_cleanup\n");
    push (@public, "  public :: set_default_rounding_mode\n");
    push (@{$funint{"get_str"}}, "    module procedure get_str_rnd\n");
    push (@{$funint{"fmpfr"}}, "    module procedure fun_set_str\n");

    &call_init ("long");
    $olines .= "#if SIZEOF_INT < SIZEOF_LONG\n";
    push(@{$funint{"init"}}, "#if SIZEOF_INT < SIZEOF_LONG\n");
    &call_init ("int");
    push(@{$funint{"init"}}, "#endif\n");
    $olines .= "#endif\n";
    &call_init ("short");
    push(@{$funint{"init"}}, "    module procedure init_default\n");
}

# A few routines which take different integer arguments.

sub call_init
{
    my ($type) = @_;
    $olines .= <<"EOF";
  elemental subroutine init_$type (op, prec)
    type (fmpfr), intent(inout) :: op
    integer (c_$type), intent(in) :: prec
    integer (mpfr_prec_kind) :: prec_val
    prec_val = prec
    if (op%initialized) then
      call mpfr_set_prec (op%mp, prec_val)
    else
      call mpfr_init2 (op%mp, prec_val)
    end if
   end subroutine init_$type

  subroutine set_default_prec_$type (prec)
    $ftype{$type}, intent(in) :: prec
    default_prec = prec
  end subroutine set_default_prec_$type

  function fun_set_str_$type (s, prec, rnd) result(rop)
    type (fmpfr) :: rop
    character(kind=c_char,len=*), intent(in) :: s
    integer (c_$type), intent(in) :: prec
    integer (kind=int8), intent(in), optional :: rnd
    integer (mpfr_prec_kind) :: prec_val
    integer(c_int) :: rnd_val
    character(kind=c_char), dimension(:), allocatable, target :: s_arg

    prec_val = prec
    rnd_val = default_rnd
    if (present(rnd)) rnd_val = rnd
    if (.not. rop%initialized) then
       call mpfr_init2 (rop%mp, prec_val)
       rop%initialized = .true.
     end if
    
    allocate (s_arg(len(s)+1))
    s_arg(1:len(s)) = transfer(s,s_arg)
    s_arg(len(s)+1) = char(0)
    call fmpfr_set_str (rop%mp, c_loc(s_arg), 10, rnd_val)
    deallocate (s_arg)
  end function fun_set_str_$type

  function get_str_$type (op, prec, rnd) result(res)
    type (fmpfr), intent(in) :: op
    $ftype{$type}, intent(in) :: prec
    integer (kind=int8), intent(in), optional :: rnd
    character (len=:), allocatable :: res
    integer (kind=c_size_t) :: prec_val

    prec_val = prec
    res = get_str_work (op, prec_val, rnd)
  end function get_str_$type


EOF

    push(@{$funint{"init"}}, "    module procedure init_$type\n");
    push(@{$funint{"set_default_prec"}},
	 "    module procedure set_default_prec_$type\n");
    push(@{$funint{"fmpfr"}}, "    module procedure fun_set_str_$type\n");
    push(@{$funint{"get_str"}},"    module procedure get_str_$type\n");
}

sub write_glue
{
    my ($intent) = @_;
    my (@a, $fg, $fname, $at);
    $intent = "intent(out)" unless defined($intent);
    print CGLUE "#if USE_FLOAT128\n" if $is_float128;
    print CGLUE  "#if USE_LONG_DOUBLE\n" if $is_long_double;
    $fname = "f" . $name;
    print CGLUE "void $fname (";
    for my $i (0..$#argtypes) {
	push (@a, $argtypes[$i] . " " . $vars[$i]);
    }
    print CGLUE join(", ",@a),")\n{\n";
    print CGLUE "  $name (", join(", ",@vars),");\n";
    print CGLUE "}\n\n";
    print CGLUE "#endif\n" if $is_float128 || $is_long_double;

    $fg = "";
    $fg .= "#if USE_FLOAT128\n" if $is_float128;
    $fg .= "#if USE_LONG_DOUBLE\n" if $is_long_double;
    $fg .= "    pure subroutine $fname (" . join(", ",@vars) . ") bind(c)\n";
    $fg .= "      import\n";
    $fg .= "      " . $atype{$argtypes[0]} . ", $intent :: " . $vars[0] . "\n";
    for $i (1..$#argtypes) {
	$at = $atype{$argtypes[$i]};
	$fg .= "      $at";
	$fg .= ", intent(in) " unless $at =~ m/value/;
	$fg .= ":: " . $vars[$i] . "\n";
    }
    $fg .= "    end subroutine $fname\n\n";
    $fg .= "#endif\n" if $is_float128 || $is_long_double;
    $fglue .= $fg;
}

sub handle_compare
{
    my($comparison) = @_;
    my ($nlines, $xname, $operator);

    $xname = $comparison . "_p";
    $operator = $compare{$comparison};
    $nlines .= <<"EOF";
    elemental function $xname (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      rc = $name (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function $xname

EOF

    $olines .= $nlines;
    push (@{$compare_int{$operator}}, "    module procedure $xname\n");
}
