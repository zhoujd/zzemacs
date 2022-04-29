#!/bin/bash

if [ -z $(command -v evolution) ] ; then
    echo "Cannot find evolution"
    exit 1
fi

force_down() {
    echo "evolution down"
    evolution --force-shutdown > /dev/null 2>&1
    sleep 2s
}

disable_alarm() {
    echo "evolution noalarm"
    echo "kill evolution-alarm-notify"
    sudo killall evolution-alarm
    echo "chmod -x evolution-alarm-notify"
    sudo chmod -x /usr/libexec/evolution-data-server/evolution-alarm-notify
}

case $1 in
    force )
        force_down
        ;;
    noalarm )
        disable_alarm
        ;;

    * )
        echo "Usage: $(basename $0) {force|noalarm}"
        ;;
esac
