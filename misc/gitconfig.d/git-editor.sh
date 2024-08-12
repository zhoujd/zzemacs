#!/bin/sh

case "$TERM" in
    "dumb" )
        echo "Please usage: git commit -am \"message for commit\""
        ;;
    * )
        if [ -x "$(command -v emacs)" ]; then
            emacs -nw -Q $*
        else
            vim $*
        fi
        ;;
esac
