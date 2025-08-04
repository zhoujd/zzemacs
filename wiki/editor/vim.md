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
