PERL
====

1. Build perl from source

        $ wget http://www.cpan.org/src/5.0/perl-5.20.1.tar.gz
        $ tar -xzf perl-5.20.1.tar.gz
        $ cd perl-5.20.1
        $ ./Configure -des -Dprefix=$HOME/local
        $ make
        $ make test
        $ make install

