#!/bin/sh

case "$TERM" in
    "dumb" )
        echo "Please usage: git commit -am \"message for commit\""
        ;;
    * )
        emacs -nw -Q
        ;;
esac
