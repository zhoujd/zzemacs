#!/bin/bash

case $1 in
    srv )
        echo "Run srv mode"
        git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose
        ;;
    hub )
        echo "Run hub mode"
        git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose
        ;;
    * )
        echo "Usage: $(basename $0) {srv|hub}"
        ;;
esac
