#!/bin/sh


SETUP_ROOT=`pwd`

##Import vars and functions
. $SETUP_ROOT/sample.sh

echo "Setup self .bashrc start ..."

BASHRC_PATH=~/.bashrc
SELF_BASHRC_PATH=$(cd $SETUP_ROOT/../misc && pwd)

##setup .bashrc
Install_self_bashrc()
{
try_command cat >> $BASHRC_PATH <<EOF
## source self .bashrc setting
if [ -f $SELF_BASHRC_PATH/.bashrc ] ; then
    . ${SELF_BASHRC_PATH}/.bashrc
fi
EOF
}

if [ -f $SELF_BASHRC_PATH/.bashrc ] ; then
    Install_self_bashrc
fi

echo "Setup self .bashrc end ..."
