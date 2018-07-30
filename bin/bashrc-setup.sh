#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

echo "Setup self .bashrc start ..."

##setup .bashrc
install_self_bashrc() {
cat <<EOF >> ~/.bashrc

# zzemacs bash configure
if [ -f ~/.bash_zzemacs ]; then
    . ~/.bash_zzemacs
fi
EOF
}

install_self_bashrc

echo "Setup self .bashrc end ..."
