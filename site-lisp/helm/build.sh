#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ASYNC_ROOT=$(cd $SCRIPT_ROOT/../emacs-async && pwd)

export EMACSLOADPATH="$ASYNC_ROOT:"
make
