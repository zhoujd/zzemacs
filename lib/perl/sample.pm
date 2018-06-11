package sample;

require Exporter;
our @ISA         = qw(Exporter);
our @EXPORT      = qw(&run_cmds is_windows);

use warnings;
use strict;
use Getopt::Std;
use Cwd;

use config;

$| = 1;

sub is_windows {
    if ("$ENV{'OS'}" eq "Windows_NT") {
        return 1;
    } else {
        return 0;
    }
}

sub run_cmds {
    my @cmds = @_;
    foreach (@cmds) {
        print "$_\n" if $conf{'verb'}; 
        unless ("" eq $_) {
            (system("$_") == 0) || die "can't run $_ : $!";
        } 
    }
}

1;
