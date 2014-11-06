#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

##Get Script path
SETUP_ROOT=$(cd $(dirname $0) && pwd)

##Import vars and functions
. $SETUP_ROOT/sample.sh

echo "Setup self .bashrc start ..."

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)
BASHRC_PATH=$HOME/.bashrc

##setup .bashrc
Install_self_bashrc()
{
try_command cat >> $BASHRC_PATH <<EOF
# self bash-setting from zzemacs
if [ -d ${ZZEMACS_ROOT} ] ; then
    export PATH=${ZZEMACS_ROOT}/bin:${ZZEMACS_ROOT}/libexec:\$PATH
    for i in ${ZZEMACS_ROOT}/etc/profile.d/*.sh ; do
        if [ -r "\$i" ]; then
            if [ "\${-#*i}" != "\$-" ]; then
                . "\$i"
            else
                . "\$i" >/dev/null 2>&1
            fi
        fi
    done
fi
EOF
}

Install_self_bashrc

echo "Setup self .bashrc end ..."
