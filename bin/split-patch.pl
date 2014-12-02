#!/usr/bin/env perl

use strict;
use Cwd;

sub help {
    print "Usage: $0 diff_a diff_b\n";
    exit 1;
}

sub main {
    print "hello, zhoujd\n";

    help() if (@ARGV != 2);

    my $current_dir = getcwd();
    my $split_dir   = "$current_dir/out";

    my $diff_a = $ARGV[0];
    my $diff_b = $ARGV[1];

    my @diff_stat = `git diff --stat $diff_a $diff_b`;
    my $index  = 0;

    foreach my $i (0 .. @diff_stat - 2) {
        my $line = $diff_stat[$i];
        chomp($line);
        $line =~ s/\|.*$//;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;

        my $file_diff_name = $line;
        $file_diff_name =~ s|.*/||;  # remove folder only file

        my $index_prefix = sprintf("%04d", ++$index);
        my $cmd = "git diff $diff_a -- $file_diff_name $diff_b -- $file_diff_name > $split_dir/${index_prefix}-${file_diff_name}.diff";
        print "$cmd\n";

        (system("mkdir -p $split_dir") == 0) || die "can`t run $cmd $!";
        (system($cmd) == 0) || die "can`t run $cmd $!";

        print "== $line\n";
    }

    printf "patches to %d files finished.\n", $index;
}

main;
