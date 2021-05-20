#!/bin/bash
#set -x

if [ $# != 2 ]; then
    echo "Usage: $0 <user@host> <set|get>"
    exit 1
fi

HOST=$1
TYPE=$2

NOW=$(date +"%Y-%m-%d %T")
echo "Now: $NOW"

date_set() {
    echo "DATE: Set $HOST $NOW"
    ssh -T $HOST << EOSSH
date --set \"$NOW\""
EOSSH
}

date_get() {
    ssh -T $HOST << 'EOSSH'
echo "DATE: $(date) [$(hostname)]"
EOSSH
}

case "$TYPE" in
    "set" )
        date_set
        ;;
    "get" )
        date_get
        ;;
    * )
        echo "Unknown type: $TYPE"
        exit 1
        ;;
esac

echo "ssh time sync done"
