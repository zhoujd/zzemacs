#!/bin/bash
#
# Startup script for mercurial server.
#
# Change following lines

APP_BIN=hg
SRC=`pwd`

SRCNAME="package source"
# Path to PID file of running mercurial process.
PID_FILE=$HOME/hg-serve.pid

STATE=$1
if [ ! $2 = "" ]; then
    PORT=$2
else
    PORT=8000
fi

case "${STATE}" in
    'start')
        echo "Mecurial Server service starting on port:${PORT}."
        (cd ${SRC}; ${APP_BIN} serve --name "${SRCNAME}" -d -p ${PORT} --pid-file ${PID_FILE})
        ;;

    'stop')
        if [ -e "${PID_FILE}" ]; then
            PID=`cat "${PID_FILE}"`
            if [ "${PID}" -gt 1 ]; then
                kill -TERM ${PID}
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
                kill -TERM ${PID}
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
