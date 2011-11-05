#!/usr/bin/env perl

use strict;
use PDF::FDF::Simple;

use Getopt::Std;

our $opt_o;

getopts("o:");



my $outfile=$opt_o || "-";


my %content=();
our $fields;

foreach my $file (@ARGV){
  open(IN,"$file") || die ("could not open $file");
  undef($/);
  my $in=<IN>;
  eval($in);
  die "reading input failed: $@" if $@;

  %content=(%content, %$fields);
}


my $f=new PDF::FDF::Simple({
			    filename => $outfile,
			    content => \%content });

$f->save;

