PERL
====

## Build perl from source

    $ VER=5.34.0
    $ wget http://www.cpan.org/src/5.0/perl-${VER}.tar.gz
    $ tar -xzf perl-${VER}.tar.gz
    $ cd perl-${vER}
    $ ./Configure -des -Dprefix=$HOME/localperl
    $ make
    $ make test
    $ make install
