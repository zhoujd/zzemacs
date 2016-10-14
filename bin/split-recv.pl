#!/usr/bin/env perl

## sudo yum install xterm


use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

my @file_trunks = qw |
    xaa xab xac xad xae xaf xag
    xah xai xaj xak xal xam xan
    xao xap xaq xar xas xat
    xau xav xaw xax xay xaz
|;

my $scp_target = ".";

sub usage
{
    print "1st remote: \$ split -b 200M <filename>\n";
    print "2nd local : \$ $0 -p <passwd> -f <remote-folder> -n <num>\n";
    print "3rd local : \$ cat xa* > target-file\n";

    exit 1;
}

sub main
{
    print "Welcome to split recv ...\n";
    usage() unless @ARGV;

    my %opt;
    getopts('f:p:n:C:', \%opt);

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
            $scp_target = $opt{C} if defined $opt{C};

            my $trans_cmd = "scp.exp $opt{p} $opt{f}/$file_trunks[$i] $scp_target";
            print "$trans_cmd\n";

            my $term_cmd = "xterm -e \"$trans_cmd\"";
            (system("$term_cmd") == 0) || die "can't exec $term_cmd: $!";
        }
        else
        {
            # fork returned 0 nor undef
            # so this branch is parent
            push @procs, $pid;
        }

    }
}

main;
