#!/usr/bin/perl -w

use Term::ReadLine;
use Data::Dumper;

my $historyfile = $ENV{HOME} . '/.phistory';
my $term = new Term::ReadLine 'Perl Shell';

sub save_list 
{ 
    my $f = shift; 
    my $l = shift; 
    open F, $f; 
    print F "$_\n" foreach @$l 
}

if (open H, $historyfile)
{
    @h = <H>;
    chomp @h;
    close H;
    $h{$_} = 1 foreach @h;
    $term->addhistory($_) foreach keys %h;
}

while ( defined ($_ = $term->readline("ZZ Perl Shell> ")) ) 
{
    my $res = eval($_);
    warn $@ if $@;
    unless ($@)
    {
        open H, ">>$historyfile";
        print H "$_\n";
        close H;
        print "\n", Data::Dumper->Dump([$res], ['Result']);
    }
    $term->addhistory($_) if /\S/;
}
