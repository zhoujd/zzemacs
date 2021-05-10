clang complete
==============

1 Ubuntu LTS
    Install llvm-dev and libclang-dev.

    $ sudo apt-get install -y clang llvm-dev libclang-dev
    $ git clone https://github.com/Golevka/emacs-clang-complete-async
    $ cd emacs-clang-complete-async
    $ make
    $ cp clang-complete ~/bin/
    $ cp auto-complete-clang.el ~/.elisp/

2 CentOS
    Install llvm-devel, clang-devel and zlib.

    $ sudo yum install -y llvm-devel clang-devel zlib
    $ git clone https://github.com/Golevka/emacs-clang-complete-async
    $ cd emacs-clang-complete-async
    $ make
    $ cp clang-complete ~/bin/
    $ cp auto-complete-clang.el ~/.elisp/

3 OSX
    Because XCode does not have enought Clang header files, install clang and llvm with Homebrew.

    $ brew install --with-clang --all-targets --rtti --universal --jit llvm
    $ git clone https://github.com/Golevka/emacs-clang-complete-async
    $ cd emacs-clang-complete-async
    $ make LLVM_CONFIG=/usr/local/opt/llvm/bin/llvm-config CC=/usr/local/opt/llvm/bin/clang
    $ cp clang-complete ~/bin/
    $ cp auto-complete-clang.el ~/.elisp/
