llvm
====

## Getting Started: Building and Running Clang

    ## https://clang.llvm.org/get_started.html
    $ git clone --depth=1 https://github.com/llvm/llvm-project.git
    $ cd llvm-project
    $ mkdir build ## (in-tree build is not supported)
    $ cd build
    $ cmake -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" ../llvm
    $ make
    $ clang --help
    $ clang file.c -fsyntax-only (check for correctness)
    $ clang file.c -S -emit-llvm -o - (print out unoptimized llvm code)
    $ clang file.c -S -emit-llvm -o - -O3
    $ clang file.c -S -O3 -o - (output native machine code)

## Easy steps for installing GCC 7.1.0

    ## https://llvm.org/docs/GettingStarted.html
    % gcc_version=7.1.0
    % wget https://ftp.gnu.org/gnu/gcc/gcc-${gcc_version}/gcc-${gcc_version}.tar.bz2
    % wget https://ftp.gnu.org/gnu/gcc/gcc-${gcc_version}/gcc-${gcc_version}.tar.bz2.sig
    % wget https://ftp.gnu.org/gnu/gnu-keyring.gpg
    % signature_invalid=`gpg --verify --no-default-keyring --keyring ./gnu-keyring.gpg gcc-${gcc_version}.tar.bz2.sig`
    % if [ $signature_invalid ]; then echo "Invalid signature" ; exit 1 ; fi
    % tar -xvjf gcc-${gcc_version}.tar.bz2
    % cd gcc-${gcc_version}
    % ./contrib/download_prerequisites
    % cd ..
    % mkdir gcc-${gcc_version}-build
    % cd gcc-${gcc_version}-build
    % $PWD/../gcc-${gcc_version}/configure --prefix=$HOME/toolchains --enable-languages=c,c++
    % make -j$(nproc)
    % make install

## Configure build of LLVM to use the new toolchain

    % mkdir build
    % cd build
    % CC=$HOME/toolchains/bin/gcc CXX=$HOME/toolchains/bin/g++ \
      cmake .. -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$HOME/toolchains/lib64 -L$HOME/toolchains/lib64"

## An Example Using the LLVM Tool Chain

    ## 1. First, create a simple C file, name it ‘hello.c’
    #include <stdio.h>

    int main() {
      printf("hello world\n");
      return 0;
    }

    ## 2. Next, compile the C file into a native executable:
    % clang hello.c -o hello

    ## 3. Next, compile the C file into an LLVM bitcode file:
    % clang -O3 -emit-llvm hello.c -c -o hello.bc

    ## 4. Run the program in both forms. To run the program, use:
    % ./hello
    and
    % lli hello.bc

    ## 5. Use the llvm-dis utility to take a look at the LLVM assembly code:
    % llvm-dis < hello.bc | less

    ## 6. Compile the program to native assembly using the LLC code generator:
    % llc hello.bc -o hello.s

    ## 7. Assemble the native assembly language file into a program:
    % /opt/SUNWspro/bin/cc -xarch=v9 hello.s -o hello.native   # On Solaris
    % gcc hello.s -o hello.native                              # On others

    ## 8. Execute the native code program:
     ./hello.native

## Extending LLVM

    ## https://llvm.org/docs/UserGuides.html
    ## https://llvm.org/docs/ExtendingLLVM.html#intrinsic-function

## LLVM Tutorial

    ## https://llvm.org/docs/tutorial/index.html
