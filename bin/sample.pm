#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Std;
use Cwd;
use FindBin;

$| = 1;

sub is_windows {
    if ("$ENV{'OS'}" eq "Windows_NT") {
        return 1;
    } else {
        return 0;
    }
}

