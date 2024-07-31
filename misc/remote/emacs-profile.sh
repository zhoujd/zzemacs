#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

echo setup emacs remote start ...

ln -sfv $SCRIPT_ROOT/emacs/emacs_profile $HOME/.emacs_profile
ln -sfv $SCRIPT_ROOT/emacs/emacs_term $HOME/.emacs_term

echo setup emacs remote end ...
