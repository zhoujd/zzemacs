#!/bin/sh

rc_local_install() {
    sudo tee /etc/rc.local <<EOF
#!/bin/sh
### BEGIN INIT INFO
# Provides:          rc.local
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
    rc_local_log=/var/log/zz-rc.log
    rm -f \$rc_local_log
    for i in /etc/rc.local.d/* ; do
        if [ -x "\$i" ]; then
           "\$i start"
           echo "\$(date) \$i $* [\$?]" >> \$rc_local_log
        fi
    done
}

do_stop() {
    rc_local_log=/var/log/zz-rc.log
    rm -f \$rc_local_log
    for i in /etc/rc.local.d/* ; do
        if [ -x "\$i" ]; then
           "\$i stop"
           echo "\$(date) \$i $* [\$?]" >> \$rc_local_log
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
