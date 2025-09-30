VIM
===

## Copy from vim to clipboard

```
## Check clipboard is activated
$ vim --version | grep 'clipboard'

## If "-clipboard" then install vim again with the "clipboard" functionality
## copy into clipboard by typing (including the plus) "+y
$ sudo apt-get install vim-gtk3 -y

## Alternatively, to tell vim to use the + register as the default register,
## put set clipboard=unnamedplus in your .vimrc.
```

## Build vim from source

```
## https://github.com/ycm-core/YouCompleteMe/wiki/Building-Vim-from-source
## https://www.homecooked.nl/posts/2016-08-15-how-to-build-vim-from-scratch/
$ sudo apt install libncurses5-dev libgtk2.0-dev libatk1.0-dev \
    libcairo2-dev libx11-dev libxpm-dev libxt-dev python2-dev \
    python3-dev ruby-dev lua5.2 liblua5.2-dev libperl-dev git
$ sudo apt-get remove vim vim-runtime gvim
$ git clone https://github.com/vim/vim.git
$ cd vim
$ ./configure --with-features=huge \
              --enable-multibyte \
              --enable-rubyinterp=yes \
              --enable-python3interp=yes \
              --with-python3-config-dir=$(python3-config --configdir) \
              --enable-perlinterp=yes \
              --enable-luainterp=yes \
              --enable-gui=gtk2 \
              --enable-cscope \
              --prefix=/usr/local
$ make VIMRUNTIMEDIR=/usr/local/share/vim/vim91
$ sudo make install
```
