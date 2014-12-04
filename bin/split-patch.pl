#!/usr/bin/env perl

use strict;
use Cwd;

# output folder name
my $out_folder = "split-output";

sub help {
    print "Usage: $0 diff_a diff_b\n";
    exit 1;
}

sub main {
    print "hello, zhoujd\n";

    help() if (@ARGV != 2);

    my $current_dir = getcwd();
    my $split_dir   = "$current_dir/$out_folder";

    my $diff_a = $ARGV[0];
    my $diff_b = $ARGV[1];

    if ( ! -e ".git") {
        print "$0 should run under git repo root directory\n";
        exit 1;
    }

    my @diff_stat = `git diff --relative --stat $diff_a $diff_b`;

    foreach my $i (0 .. $#diff_stat - 1) {
        my $item = $diff_stat[$i];
        chomp($item);

        # split file path and file status
        my ($file_path, $file_stat) = split(/\|/, $item);
        $file_path =~ s!^\s+!!;
        $file_path =~ s!\s+$!!;

        my $file_name = $file_path;
        $file_name =~ s!.*/!!;

        my $cmp_a = $diff_a;
        my $cmp_b = $diff_b;

        # remove content to file
        if ($file_stat =~ m!.*\-!) {
            $cmp_a = "$diff_a -- $file_path";
        }

        # add new content to file
        if ($file_stat =~ m!.*\+!) {
            $cmp_b = "$diff_b -- $file_path";
        }

        # file content not changed
        if ($file_stat =~ m!\s+0!) {
            $cmp_a = "$diff_a -- $file_path";
            $cmp_b = "$diff_b -- $file_path";
        }

        my $prefix = sprintf "%04d", $i + 1;
        my $cmd = "git diff $cmp_a $cmp_b > $split_dir/$prefix-$file_name.diff";
        print "[$prefix] $cmd\n";

        (system("mkdir -p $split_dir") == 0) || die "can`t run $cmd $!";
        (system($cmd) == 0) || die "can`t run $cmd $!";
    }

    print "patches to $#diff_stat files finished.\n";
}

main;
