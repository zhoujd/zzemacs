#!/bin/sh

case "$TERM" in
    "dumb" )
        echo "Please usage: git commit -am \"message for commit\""
        ;;
    * )
        if command -v me >/dev/null; then
            me $*
        elif command -v emacs >/dev/null; then
            emacs -nw -Q $*
        elif command -v vim >/dev/null; then
            vim $*
        else
            vi $*
        fi
        ;;
esac
