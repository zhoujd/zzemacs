#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

my $verb  = "";

sub in_array {
    my %items;
    my ($arr, $search_for) = @_;
    map {$items{$_} = 1 } @$arr;
    return ($items{$search_for} eq 1) ? 1 : 0;
}

sub run_cmd {
    print "[ cmd ]: $_[0]\n" if $verb;
    open( PS,"$_[0] |" ) || die "Failed: $!\n";
    while ( <PS> ) {
        chomp( $_ );
        #s/\r//g;
        print "$_\n";
    }
    close( PS );
    return $?;
}

sub usage {
    print "First remote: split -b 200M <filename>\n";
    print "Second loccal: $0 -f remote-folder -n num\n";
}

my @file_trunks = qw |
    xaa xab xac xad xae xaf xag
    xah xai xaj xak xal xam xan
    xao xap xaq xar xas xat
    xau xav xaw xax xay xaz
|;

sub main {
    print "Welcome split recv ...\n";
    usage() unless @ARGV;

    my %opt;
    getopts('f:t:p:n:', \%opt);

    my @channel_list = ();
    for my $i (0 .. $opt{n} - 1) {
        my $cmd = "scp.exp intel123 $opt{f}/$file_trunks[$i] $opt{t}";
        run_cmd("$cmd");
    }
}

main();
