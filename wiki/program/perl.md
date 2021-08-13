PERL
====

## Build perl from source

    ## https://perlmaven.com/how-to-build-perl-from-source-code
    $ VER=5.34.0
    $ wget http://www.cpan.org/src/5.0/perl-${VER}.tar.gz
    $ tar -xzf perl-${VER}.tar.gz
    $ cd perl-${vER}
    ## with thread support
    $ ./Configure -des -Dprefix=$HOME/.zach/perl5 -Dusethreads
    $ make
    $ make test
    or
    $ TEST_JOBS=3 make test_harness
    $ make install
    $ export PATH=$HOME/.zach/perl5/bin:$PATH
