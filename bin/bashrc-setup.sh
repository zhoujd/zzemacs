#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

echo "Setup self .bashrc start ..."

##Get Script path
SETUP_ROOT=$(cd $(dirname $0) && pwd)

##Import vars and functions
. $SETUP_ROOT/sample.sh

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)
BASHRC_PATH=$HOME/.bashrc

##setup .bashrc
Install_self_bashrc()
{
try_command cat >> $BASHRC_PATH <<EOF

# self bash-setting from zzemacs
if [ -f ${ZZEMACS_ROOT}/etc/profile ] ; then
    . ${ZZEMACS_ROOT}/etc/profile
fi 
EOF
}

Install_self_bashrc

echo "Setup self .bashrc end ..."
