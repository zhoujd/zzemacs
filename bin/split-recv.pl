#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

sub usage
{
    print "First remote: split -b 200M <filename>\n";
    print "Second loccal: $0 -p <passwd> -f <remote-folder> -n <num>\n";

    exit 1;
}

my @file_trunks = qw |
    xaa xab xac xad xae xaf xag
    xah xai xaj xak xal xam xan
    xao xap xaq xar xas xat
    xau xav xaw xax xay xaz
|;

sub main
{
    print "Welcome split recv ...\n";
    usage() unless @ARGV;

    my %opt;
    getopts('f:p:n:', \%opt);

    my @procs = ();
    for my $i (0 .. $opt{n} - 1)
    {
        # start  process
        if (!defined(my $pid = fork()))
        {
            # fork returned undef, so unsuccessful
            die "Cannot fork a child: $!";
        }
        elsif ($pid == 0)
        {
            print "Start $i by child process\n";
            my $trans_cmd = "scp.exp  $opt{p} $opt{f}/$file_trunks[$i] .";
            print "$trans_cmd\n";
            exec("$trans_cmd") || die "can't exec $trans_cmd: $!";
        }
        else
        {
            # fork returned 0 nor undef
            # so this branch is parent
            push @procs, $pid;
        }

    }
}

main();
