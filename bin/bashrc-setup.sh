#!/bin/sh

if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

SETUP_ROOT=`pwd`
ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)

##Import vars and functions
. $SETUP_ROOT/sample.sh

echo "Setup self .bashrc start ..."

BASHRC_PATH=$HOME/.bashrc

##setup .bashrc
Install_self_bashrc()
{
try_command cat >> $BASHRC_PATH <<EOF
# export env ZZEMACS_HOME
export ZZEMACS_HOME=$ZZEMACS_ROOT

# source self  setting for zachary zhou
for i in $ZZEMACS_ROOT/etc/profile.d/*.sh ; do
    if [ -r "\$i" ]; then
        if [ "\${-#*i}" != "\$-" ]; then
            . "\$i"
        else
            . "\$i" >/dev/null 2>&1
        fi
    fi
done

EOF
}


Install_self_bashrc

echo "Setup self .bashrc end ..."
