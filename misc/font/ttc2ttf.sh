#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

if [ $# -ne 1 ]; then
    echo "Usage $(basename $0) /path/to/file.ttc"
    exit 1
fi

file=$1
script=$SCRIPT_ROOT/scripts/ttc2ttf.pe
fontforge -script $script  $file

echo "Extact $file finished."
