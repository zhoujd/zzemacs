#!/usr/bin/env perl

use FindBin;
use lib "$FindBin::Bin";

use sample;

my $emacs_flag = "y";

sub diff_extern {

}

sub diff_emacs {
    my $diff_a = shift;
    my $diff_b = shift;
    my $zzemacs_path = "";

    if (is_windows() == 1) {
        $zzemacs_path = "$ENV{'ZZNIX_HOME'}/home/zhoujd/zzemacs";
    } else {
        $zzemacs_path = "$ENV{'HOME'}/zzemacs";
    }

    my $cmd = "emacs --no-site-file -q \
                     --eval \"(load-file \"$zzemacs_path/elisp/ediff-sample.el\")\" \
                     --eval \"(ediff-sample-diff \"$diff_a\" \"$diff_b\")\" \
                     --eval \"(message \"emacs diff finished.\")\"";

    print $cmd."\n";
}

sub main {
    my $diff_a = shift;
    my $diff_b = shift;

    if ($emacs_flag eq "y" || $emacs_flag eq "Y") {
        diff_emacs($diff_a, $diff_b);
    } else {
        diff_extern($diff_a, $diff_b);
    }
}

main($ARGV[0], $ARGV[1]);
