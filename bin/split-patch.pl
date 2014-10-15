#!/usr/bin/env perl

use strict;
use Cwd;

# format by `diffstat *.patch` output
my @diff_file_list = qw |
|;

my $current_dir = getcwd();
my $split_dir = "$current_dir/out";

sub help {
    print "Usage: $0 diff_a diff_b\n";
    exit 1;
}


sub main {
    print "hello, zhoujd\n";

    help() if (@ARGV != 2);
    
    my $diff_a = $ARGV[0];
    my $diff_b = $ARGV[1];
    my $index  = 0;
    
    foreach  my $item (@diff_file_list) {
        $index += 1;
        
        my $file_diff_name = $item;
        $file_diff_name =~ s|.*/||;

        my $index_prefix = sprintf("%04d", $index);
        my $cmd = "git diff $diff_a -- $item $diff_b -- $item > $split_dir/${index_prefix}_${file_diff_name}_diff";
        
        
        print "$cmd\n";

        (system("mkdir -p $split_dir") == 0) || die "can`t run $cmd $!";
        (system($cmd) == 0) || die "can`t run $cmd $!";
    }

    print "patches to $index files finished.\n";    
}

main;
