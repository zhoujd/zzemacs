#!/usr/bin/env perl

### Xterm is need for this script
## sudo yum install xterm
## sudo apt-get install xterm

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin qw($Bin);

$| = 1;

my @file_trunks = qw |
    xaa xab xac xad xae xaf xag
    xah xai xaj xak xal xam xan
    xao xap xaq xar xas xat
    xau xav xaw xax xay xaz
|;

my $verbose    = 0;
my $scp_target = ".";
my $scp_bin    = "$Bin/scp.exp";
my $term_bin   = "xterm -e";

my @procs = ();


sub usage
{
    print "1st remote: \$ split -b 200M <filename>\n";
    print "2nd local : \$ $0 -p <passwd> -f <user\@host:R-folder> -n <num> [-C <L-folder>]\n";
    print "3rd local : \$ cat xa* > target-file\n";

    exit 1;
}

sub run_cmd
{
    my $cmd = shift;
    (system("$cmd") == 0) || die "can't exec $cmd: $!";
}

sub run_cmds
{
    my @cmds = @_;
    foreach (@cmds)
    {
        print "$_\n" if $verbose;
        unless ("" eq $_)
        {
            (system("$_") == 0) || die "can't exec $_: $!";
        }
    }
}

sub catch_zap
{
    ## kill xterm procs
    foreach my $proc (@procs)
    {
        my $cmd = "kill -9 $proc";
        run_cmd($cmd);
    }

    my $clean_cmd = "rm -f $scp_target/xa*";
    run_cmd($clean_cmd);
}

sub main
{
    print "Welcome to split recv ...\n";
    usage() unless @ARGV;

    $SIG{INT} = \&catch_zap;

    my %opt;
    getopts('p:f:n:C:', \%opt) or die "Error in command line arguments\n";

    if (!defined($opt{p}) || !defined($opt{f}) || !defined($opt{n}))
    {
        print "Usage: $0 -p <passwd> -f <user\@host:R-folder> -n <num> [-C <L-folder>]\n";
        exit 1;
    }

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

            my $trans_cmd = "$scp_bin $opt{p} $opt{f}/$file_trunks[$i] $scp_target";
            print "$trans_cmd\n";

            my $term_cmd = "$term_bin \"$trans_cmd\"";
            exec("$term_cmd") || die "can't exec $term_cmd: $!";
        }
        else
        {
            # fork returned 0 nor undef
            # so this branch is parent
            push @procs, $pid;
        }

    }

    print "Wait all scp channel finished ...\n";
    foreach my $proc (@procs)
    {
        my $ret = waitpid($proc, 0);
        print "Completed process pid: $ret\n";
    }
}

main;
