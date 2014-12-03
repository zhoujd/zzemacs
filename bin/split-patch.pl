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
        my $item = $diff_stat[$i];
        chomp($item);

        my $file_path = $item;
        $file_path =~ s!\|.*$!!;          # trim file diff status
        $file_path =~ s!^\s+!!;           # trim head space
        $file_path =~ s!\s+$!!;           # trim tail space

        my $file_diff_name = $file_path;
        $file_diff_name =~ s!.*/!!;       # trim folder only file

        my $cmp_diff_a = $diff_a;
        my $cmp_diff_b = $diff_b;
        if ($item =~ m!\|.*\-!) {
            $cmp_diff_a = "$diff_a -- $file_path";
        }

        if ($item =~ m!\|.*\+!) {
            $cmp_diff_b = "$diff_b -- $file_path"
        }

        my $pre_index = sprintf "%04d", ++$index;
        my $cmd = "git diff $cmp_diff_a $cmp_diff_b > $split_dir/$pre_index-$file_diff_name.diff";

        print "[$pre_index] $cmd\n";

        (system("mkdir -p $split_dir") == 0) || die "can`t run $cmd $!";
        (system($cmd) == 0) || die "can`t run $cmd $!";
    }

    print "patches to $index files finished.\n";
}

main;
