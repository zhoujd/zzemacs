#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/.. && pwd)
TERMINFO_DIR=$ZZEMACS_HOME/etc/terminfo

for i in $(ls $TERMINFO_DIR); do
    sudo mkdir -p /etc/terminfo/$i
    sudo cp -vf $TERMINFO_DIR/$i/* /etc/terminfo/$i
    echo "copy $TERMINFO_DIR/$i done"
done

echo "terminfo setup done"
