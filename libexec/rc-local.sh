#!/bin/sh

rc_local_install() {
    if [ -f /etc/rc.local ]; then
        echo "/etc/rc.local already exist"
        exit 1
    else
        sudo tee /etc/rc.local <<EOF
## rc.local run as root during boot up
# run script in /etc/rc.local.d
for i in /etc/rc.local.d/* ; do
    if [ -x "\$i" ]; then
       "\$i"
    fi
done

exit 0
EOF
        sudo chmod +x /etc/rc.local
        sudo mkdir -p /etc/rc.local.d
    fi
}

rc_local_uninstall() {
    sudo rm -rfv /etc/rc.local.d
    sudo rm -rfv /etc/rc.local
}

case $1 in
    install )
        rc_local_install
        ;;
    uninstall )
        rc_local_uninstall
        ;;
    * )
        echo "$(basename $0) {install|uninstall}"
        ;;
esac

        
