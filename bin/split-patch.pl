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
    my $index     = 0;

    foreach my $i (0 .. $#diff_stat - 1) {
        my $file_diff_name = $diff_stat[$i];
        chomp($file_diff_name);
        $file_diff_name =~ s!\|.*$!!;     # trim file diff status
        $file_diff_name =~ s!^\s+!!;      # trim head space
        $file_diff_name =~ s!\s+$!!;      # trim tail space
        $file_diff_name =~ s!.*/!!;       # remove folder only file

        my $cmd = sprintf("git diff %s %s > $split_dir/%04d-$file_diff_name.diff",
                          "$diff_a -- $file_diff_name",
                          "$diff_b -- $file_diff_name",
                          ++$index);
        printf "[%04d] $cmd\n", $index;

        (system("mkdir -p $split_dir") == 0) || die "can`t run $cmd $!";
        (system($cmd) == 0) || die "can`t run $cmd $!";
    }

    print "patches to $index files finished.\n";
}

main;
