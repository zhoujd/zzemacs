#!/bin/sh

rc_zach_install() {
    sudo tee /etc/init.d/rc.zach <<EOF
#!/bin/sh
### BEGIN INIT INFO
# Provides:          rc.zach
# Required-Start:    \$all
# Required-Stop:     \$all
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Run /etc/rc.local if it exist
### END INIT INFO

PATH=/sbin:/usr/sbin:/bin:/usr/bin

. /lib/init/vars.sh
. /lib/lsb/init-functions

do_start() {
    rc_zach_log=/var/log/zz-rc.log
    rm -f \$rc_zach_log
    for i in /etc/rc.zach.d/* ; do
        if [ -x "\$i" ]; then
           "\$i" start
           echo "\$(date) \$i $* [\$?]" >> \$rc_zach_log
        fi
    done
}

do_stop() {
    rc_zach_log=/var/log/zz-rc.log
    rm -f \$rc_zach_log
    for i in /etc/rc.zach.d/* ; do
        if [ -x "\$i" ]; then
           "\$i" stop
           echo "\$(date) \$i $* [\$?]" >> \$rc_zach_log
        fi
    done
}

case "\$1" in
    start)
    	do_start
        ;;
    restart|reload|force-reload)
        echo "Error: argument '\$1' not supported" >&2
        exit 3
        ;;
    stop)
        do_stop
        ;;
    *)
        echo "Usage: \$0 {start|stop}" >&2
        exit 3
        ;;
esac
EOF
    sudo chmod +x /etc/init.d/rc.zach
    sudo ln -sf /etc/init.d/rc.zach /etc/rc.local
    sudo mkdir -p /etc/rc.zach.d
}

rc_zach_uninstall() {
    sudo rm -rfv /etc/rc.zach.d
    sudo rm -rfv /etc/rc.zach
}

case $1 in
    install )
        rc_zach_install
        ;;
    uninstall )
        rc_zach_uninstall
        ;;
    * )
        echo "$(basename $0) {install|uninstall}"
        ;;
esac
