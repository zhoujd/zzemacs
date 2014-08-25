#!/bin/sh

if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

SETUP_ROOT=`pwd`

##Import vars and functions
. $SETUP_ROOT/sample.sh

echo "Setup self .bashrc start ..."

BASHRC_PATH=$HOME/.bashrc
SELF_BASHRC_PATH=$(cd $SETUP_ROOT/../misc && pwd)
SELF_BIN_PATH=$(cd $SETUP_ROOT/../bin && pwd)

##setup .bashrc
Install_self_bashrc()
{
try_command cat >> $BASHRC_PATH <<EOF
# add for LS_COLORS
alias ls='ls --color=auto'

# add zzemacs/bin to PATH for zachary zhou
if [ -d ${SELF_BIN_PATH} ] ; then
    export PATH=${SELF_BIN_PATH}:\$PATH
fi

# source self .bashrc setting for zachary zhou
if [ -f ${SELF_BASHRC_PATH}/.bashrc ] ; then
    . ${SELF_BASHRC_PATH}/.bashrc
fi
EOF
}

if [ -f $SELF_BASHRC_PATH/.bashrc ] ; then
    Install_self_bashrc
fi

echo "Setup self .bashrc end ..."
