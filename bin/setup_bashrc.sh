#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

echo "Setup self .bashrc start ..."

##setup .bashrc
Install_self_bashrc()
{
cat >> ~/.bashrc <<EOF

# zzemacs bash-setting
test -f ~/.bash_zzemacs && . ~/.bash_zzemacs
EOF
}

Install_self_bashrc

echo "Setup self .bashrc end ..."
