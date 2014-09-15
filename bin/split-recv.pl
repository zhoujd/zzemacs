#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

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
        my $cmd = "xterm -e scp.exp  $opt{p} $opt{f}/$file_trunks[$i] $opt{t} &";
        (system("$cmd") == 0) || die "Cannot run $cmd $!";
    }
}

main();
