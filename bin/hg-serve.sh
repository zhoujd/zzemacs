#!/bin/bash
#
# Startup script for mercurial server.
#
# Change following lines

APP_BIN=hg

SRCNAME="package source"

STATE=$1
if [ ! $2 = "" ]; then
    PORT=$2
else
    PORT=8000
fi

# kill command line
if [ "$OS" = "Windows_NT" ] ; then
    SRC=`pwd -W`
    KILL_CMD="taskkill //F //PID"

    # For cmd run
    if [ "$HOME" = "" ]; then
        HOME="c:/zznix/home/zhoujd"
    fi
else
    SRC=`pwd`
    KILL_CMD="kill -TERM"
fi

# Path to PID file of running mercurial process.
PID_FILE=$HOME/hg-serve.pid

# process operate
case "${STATE}" in
    'start')
        echo "Mecurial Server service starting on port:${PORT}."
        (cd ${SRC}; ${APP_BIN} serve --name "${SRCNAME}" -d -p ${PORT} --pid-file ${PID_FILE})
        ;;

    'stop')
        if [ -e "${PID_FILE}" ]; then
            PID=`cat "${PID_FILE}"`
            if [ "${PID}" -gt 1 ]; then
                $KILL_CMD ${PID}
                rm -f ${PID_FILE}
                echo "Stopping the Mercurial service PID=${PID}."
            else
                echo Bad PID for Mercurial -- \"${PID}\"
            fi
        else
            echo No PID file recorded for mercurial
        fi
        ;;

    'restart')
        if [ -e "${PID_FILE}" ]; then
            PID=`cat "${PID_FILE}"`
            if [ "${PID}" -gt 1 ]; then
                $KILL_CMD ${PID}
                rm -f ${PID_FILE}
                echo "Stopping the Mercurial service PID=${PID}."
            fi
        fi

        echo "Mecurial Server service restarting on port:${PORT}."
        (cd ${SRC}; ${APP_BIN} serve --name "${SRCNAME}" -d -p ${PORT} --pid-file ${PID_FILE})
        ;;
    *)
        echo "$0 {start [port]|stop|restart [port]}"
        exit 1
        ;;
esac
