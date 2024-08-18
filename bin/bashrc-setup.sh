#!/bin/sh

echo "Setup self .bashrc start ..."

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)
ZZEMACS_BASHRC=~/.bashrc.d/zz-emacs.sh

##setup .bashrc
install_bashrc() {
    mkdir -p ~/.bashrc.d
    cat <<EOF >> ~/.bashrc

# load script in ~/.bashrc.d
for i in ~/.bashrc.d/*.sh ; do
    [ -r "\$i" ] && . "\$i" >/dev/null 2>&1
done
EOF
}

install_bashrc_emacs() {
    cat <<EOF > $ZZEMACS_BASHRC
[ -f $ZZEMACS_ROOT/etc/profile ] && . $ZZEMACS_ROOT/etc/profile
EOF
}

install_bashrc
install_bashrc_emacs

echo "Setup self .bashrc end ..."
