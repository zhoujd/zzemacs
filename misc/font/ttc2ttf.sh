#!/bin/bash

if [ $# < 1 ]; then
    echo "Usage $(basename $0) /path/to/file.ttc"
    exit 1
fi

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
fontforge -script $SCRIPT_ROOT/scripts/ttc2ttf.pe $1
