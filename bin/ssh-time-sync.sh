#!/bin/bash
#set -x

if [ $# != 1 ]; then
    echo "Usage: $0 <user@host>"
    exit 1
fi

HOST=$1

NOW=$(date +"%Y-%m-%d %T")
echo "Now: $NOW"

ssh -T $HOST << EOSSH
echo "DATE: Set $HOST $NOW"
date --set \"$NOW\""
EOSSH

ssh -T $HOST << 'EOSSH'
echo "DATE: $(date) [$(hostname)]"
EOSSH
