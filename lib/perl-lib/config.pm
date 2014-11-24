package config;

require Exporter;
our @ISA         = qw(Exporter);
our @EXPORT      = qw(%conf);

our %conf = (
    "verb" => 0,
);

1;
