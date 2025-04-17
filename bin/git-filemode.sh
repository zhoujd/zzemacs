#!/bin/sh

## core.filemode

case "$1" in
    "windows" )
        git config core.fileMode false
        ;;
    "linux" )
        git config core.fileMode true
        ;;
    * )
        echo "Use: $0 [windows|linux]"
        ;;
esac
