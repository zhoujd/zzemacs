#!/usr/bin/env perl

use warnings;
use strict;
use FindBin qw($Bin);


$| = 1;

sub diff_emacs {
    my $diff_a = shift;
    my $diff_b = shift;
    
    my $zzemacs_path = "~/zzemacs";

    # Reset path on linux
    if ( "$^O" eq "linux" ) {
        $zzemacs_path = "$Bin/..";
    } elsif ( "$^O" eq "msys" ) {
        $zzemacs_path = "$ENV{ZZNIX_HOME}/home/zhoujd/zzemacs";
    }
    
    my $elisp_string = sprintf("(progn \
                                    (load-file \\\"%s/elisp/ediff-sample.el\\\") \
                                    (ediff-sample-diff \\\"%s\\\" \\\"%s\\\") \
                                )", $zzemacs_path, $diff_a, $diff_b);

    my $cmd = sprintf("emacs -nw -q --no-site-file --eval \"%s\"", $elisp_string);

    (system("$cmd") == 0) || die "Cannot run $cmd $!";
}

sub main {
    diff_emacs($ARGV[0], $ARGV[1]);
}

main;
