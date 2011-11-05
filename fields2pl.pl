#!/usr/bin/env perl 

my ($LOOKING,$INSTATE)=(0,1);

my $state=$LOOKING;

while(<>){
  if (($state==$LOOKING) && m/---/){
    $state=$INSTATE;
  }
  if (($state==$INSTATE) && m/^FieldName:\s+(.*)$/){
    push(@names,$1);
    $state=$LOOKING;
  }
}

print '$fields={',"\n";
print join(",\n",map {sprintf("'%s'=>q{}",$_);} @names);
print "\n};\n";

