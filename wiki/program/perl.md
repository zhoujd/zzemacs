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

## Perl Prototypes

    ## https://perldoc.perl.org/perlsub#Prototypes
    Declared as             Called as
    sub mylink ($$)         mylink $old, $new
    sub myvec ($$$)         myvec $var, $offset, 1
    sub myindex ($$;$)      myindex &getstring, "substr"
    sub mysyswrite ($$$;$)  mysyswrite $buf, 0, length($buf) - $off, $off
    sub myreverse (@)       myreverse $a, $b, $c
    sub myjoin ($@)         myjoin ":", $a, $b, $c
    sub mypop (\@)          mypop @array
    sub mysplice (\@$$@)    mysplice @array, 0, 2, @pushme
    sub mykeys (\[%@])      mykeys $hashref->%*
    sub myopen (*;$)        myopen HANDLE, $name
    sub mypipe (**)         mypipe READHANDLE, WRITEHANDLE
    sub mygrep (&@)         mygrep { /foo/ } $a, $b, $c
    sub myrand (;$)         myrand 42
    sub mytime ()           mytime

    ## use the \[] backslash group notation to specify more than one allowed argument type.
    sub myref (\[$@%&*])
    ## it will allow calling myref() as
    myref $var
    myref @array
    myref %hash
    myref &sub
    myref *glob

## Perl LanguageServer

    ## https://marketplace.visualstudio.com/items?itemName=richterger.perl
    ## https://dev.to/perldean/vscode-as-a-perl-ide-3cco
    ## on Debian/Ubuntu
    $ sudo apt install libanyevent-perl libclass-refresh-perl \
    libdata-dump-perl libio-aio-perl libjson-perl libmoose-perl libpadwalker-perl \
    libscalar-list-utils-perl libcoro-perl
    ## On Ubuntu 22.04
    $ sudo apt install libcompiler-lexer-perl

    $ sudo cpan Perl::LanguageServer

    ## on Centos 7
    $ sudo yum install perl-App-cpanminus perl-AnyEvent-AIO perl-Coro
    $ sudo cpanm Class::Refresh
    $ sudo cpanm Compiler::Lexer
    $ sudo cpanm Hash::SafeKeys
    $ sudo cpanm Perl::LanguageServer
