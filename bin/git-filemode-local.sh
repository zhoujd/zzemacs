#!/bin/sh

## core.filemode

case "$1" in
    "windows" )
        git config core.filemode false
        ;;
    "linux" )
        git config core.filemode true
        ;;
    * )
        echo "Use: $0 [windows|linux]"
        ;;
esac
