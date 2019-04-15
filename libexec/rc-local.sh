#!/bin/sh

rc_local_install() {
    sudo tee /etc/rc.local <<EOF
#!/bin/sh -e
## rc.local run as root during boot up
# run script in /etc/rc.local.d

rc_local_log=/var/log/zz-rc.log
rm -f \$rc_local_log
for i in /etc/rc.local.d/* ; do
    if [ -x "\$i" ]; then
       "\$i"
       echo "\$(date) \$i [\$?]" >> \$rc_local_log
    fi
done

exit 0
EOF
    sudo chmod +x /etc/rc.local
    sudo mkdir -p /etc/rc.local.d
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
